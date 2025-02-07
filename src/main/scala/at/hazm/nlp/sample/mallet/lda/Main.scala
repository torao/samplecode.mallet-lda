package at.hazm.nlp.sample.mallet.lda

import java.io.File

import cc.mallet.pipe._
import cc.mallet.pipe.iterator.SimpleFileLineIterator
import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.{FeatureSequence, InstanceList}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object Main {
  def main(args:Array[String]):Unit = {

    // コマンドライン引数の解析
    val conf = parse(args.toList)

    // 学習用のインスタンス (文書) とパイプ (変換処理) を作成
    val pipes = new SerialPipes(Seq(
      new Input2CharSequence("UTF-8"), // ファイルからの文書読み込み想定
      new Japanese2TokenSequence(), // 自前の形態素解析によるトークン化
      new TokenSequence2FeatureSequence() // トークンを ID にマッピング (withBigrams 版もある)
    ).asJava)

    // モデルのファイルが存在する場合はそれを利用、存在しない場合は新規に学習
    val model = if(conf.model.exists()) {
      println("loading existing model")
      ParallelTopicModel.read(conf.model)
    } else {
      val instances = new InstanceList(pipes)
      instances.addThruPipe(new SimpleFileLineIterator(conf.train))

      // 学習の実行
      val t0 = System.currentTimeMillis()
      println(f"training: Topics=${conf.topics}%,d, Σα_t=${conf.alpha}%.2f, β=${conf.beta}%.2f, iter=${conf.iterations}%,d")
      val model = new ParallelTopicModel(
        conf.topics, if(conf.alpha < 0.0) conf.topics else conf.alpha, conf.beta)
      model.setNumIterations(conf.iterations)
      model.setNumThreads(conf.threads)
      model.addInstances(instances)
      model.estimate()
      val t1 = System.currentTimeMillis()
      println(f"${t1 - t0}%,dms training finish")

      // モデルの保存
      model.write(conf.model)

      // 学習に使用した各ドキュメントの単語が所属するらしきトピックを出力
      println("WORD TO TOPIC FOR DOCUMENTS ----")
      val corpus = model.getAlphabet
      model.getData.asScala.foreach { doc =>
        val tokens = doc.instance.getData.asInstanceOf[FeatureSequence]
        val topics = doc.topicSequence
        val scoredTerms = for(i <- 0 until tokens.getLength) yield {
          f"${corpus.lookupObject(tokens.getIndexAtPosition(i))}%s(${topics.getIndexAtPosition(i)}%d)"
        }
        println(scoredTerms.mkString(" "))
      }

      model
    }

    // コーパス (ID<->トークン変換マップ) 取得
    // val corpus = instances.getDataAlphabet
    val corpus = model.getAlphabet

    // トピックごとに特徴語を出力
    println("FEATURED WORDS FOR TOPICS ----")
    println("ID: PROB  WORDS")
    val sortedWords = model.getSortedWords
    val topicProbabilities = model.getTopicProbabilities(0)
    for(i <- 0 until sortedWords.size()) {
      val it = sortedWords.get(i).iterator().asScala
      println(f"$i%,2d: ${topicProbabilities(i)}%.3f ${
        it.take(10).map { ids =>
          f"${corpus.lookupObject(ids.getID)}%s(${ids.getWeight}%.1f)"
        }.mkString(" ")
      }")
    }

    // 既存の文書に対して最も適切と思われるトピックを推測
    println("MOST LIKELY TOPIC FOR DOCUMENTS ----")
    println("ID(PROB ): DOCUMENT")
    conf.predict.foreach { predict =>
      val inferencer = model.getInferencer
      val predictData = new InstanceList(pipes)
      predictData.addThruPipe(new SimpleFileLineIterator(predict))
      predictData.iterator().asScala.foreach { instance =>
        val probabilities = inferencer.getSampledDistribution(instance, 100, 1, 5)
        val (prob, maxTopic) = probabilities.zipWithIndex.maxBy(_._1)
        val tokens = instance.getData.asInstanceOf[FeatureSequence]
        val doc = (for(i <- 0 until tokens.getLength) yield {
          corpus.lookupObject(tokens.getIndexAtPosition(i))
        }).mkString(" ")
        println(f"$maxTopic%2s($prob%.3f): $doc%s")
      }
    }
  }

  /**
    * 動作設定。
    * @param train 学習データファイル
    * @param topics トピック数 K
    * @param alpha ハイパーパラメータ α
    * @param beta ハイパーパラメータ β
    * @param iterations イテレーション回数
    * @param threads 実行スレッド数
    * @param predict 予測データファイル
    */
  case class Config(train:File = new File("train.txt"), topics:Int = 10, alpha:Double = -1.0, beta:Double = ParallelTopicModel.DEFAULT_BETA, iterations:Int = 1000, threads:Int = 1, predict:Option[File] = None) {
    private[this] def basename:String = {
      val i = train.getName.lastIndexOf('.')
      if(i < 0) train.getName else train.getName.substring(0, i)
    }

    def model:File = {
      val dir = train.getParentFile
      new File(dir, s"$basename-t${topics}a${if(alpha < 0.0) "0" else alpha.toString}b${beta}i$iterations.lda")
    }

    def corpus:File = {
      val dir = train.getParentFile
      new File(dir, s"$basename-t${topics}a${if(alpha < 0.0) "0" else alpha.toString}b${beta}i$iterations.corpus")
    }
  }

  /**
    * コマンドラインパラメータを解析し [[Config]] を構築します。
    *
    * @param param コマンドラインパラメータ
    * @return コマンドラインで指定された設定
    */
  private[this] def parse(param:List[String]):Config = param match {
    case ("-t" | "--topics") :: topics :: rest =>
      Try(topics.toInt) match {
        case Success(t) if t > 0 => parse(rest).copy(topics = t)
        case Success(t) => usage(s"topic number must be greater than zero: $t")
        case Failure(ex) => usage(s"unexpected topics value: $topics; $ex")
      }
    case ("-a" | "--alpha") :: alpha :: rest =>
      Try(alpha.toDouble) match {
        case Success(a) => parse(rest).copy(alpha = a)
        case Failure(ex) => usage(s"unexpected alpha value: $alpha; $ex")
      }
    case ("-b" | "--beta") :: beta :: rest =>
      Try(beta.toDouble) match {
        case Success(b) => parse(rest).copy(beta = b)
        case Failure(ex) => usage(s"unexpected beta value: $beta; $ex")
      }
    case ("-i" | "--iterations") :: iter :: rest =>
      Try(iter.toInt) match {
        case Success(i) if i > 0 => parse(rest).copy(iterations = i)
        case Success(i) => usage(s"iteration number must be greater than zero: $i")
        case Failure(ex) => usage(s"unexpected iteration value: $iter; $ex")
      }
    case ("-p" | "--predict") :: predict :: rest if !predict.startsWith("-") =>
      parse(rest).copy(predict = Some(new File(predict)))
    case ("-T" | "--threads") :: threads :: rest =>
      Try(threads.toInt) match {
        case Success(t) if t > 0 => parse(rest).copy(threads = t)
        case Success(t) => usage(s"threads number must be greater than zero: $t")
        case Failure(ex) => usage(s"unexpected threads value: $threads; $ex")
      }
    case src :: rest if !src.startsWith("-") =>
      parse(rest).copy(train = new File(src))
    case ("-h" | "--help") :: _ => usage()
    case unsupported :: _ => usage(s"unsupported option: $unsupported")
    case Nil => Config()
  }

  /**
    * 標準出力に使用方法を出力し `System.exit(1)` で終了します。
    *
    * @param errorMessage エラーメッセージ (必要であれば)
    * @return (リターンすることはない)
    */
  private[this] def usage(errorMessage:String = ""):Nothing = {
    if(errorMessage.nonEmpty) {
      System.err.println(s"ERROR: $errorMessage")
    }
    System.err.println(
      s"""USAGE: scala ${getClass.getName.dropRight(1)} {OPTIONS} [train_file]
        |OPTIONS:
        |  -t, --topics [num] ............. topic number (default 10)
        |  -i, --iterations [num] ......... iteration count (default 1000)
        |  -a, --alpha [float] ............ sum of alpha values of all topics (default topic num)
        |  -b, --beta [float] ............. beta value (default ${ParallelTopicModel.DEFAULT_BETA})
        |  -T, --threads [num] ............ threads to exec (default 1)
        |  -p, --predict [predict_file] ... file that contains docs to predict topic
        |  -h, --help ..................... show this message
        |  [train_file] ................... REQUIRED file that contains docs to train model
        |The file [train_file] and [predict_file] should contain a document for its line.
        |""".stripMargin)
    System.exit(1)
    throw new Exception("exit")
  }
}
