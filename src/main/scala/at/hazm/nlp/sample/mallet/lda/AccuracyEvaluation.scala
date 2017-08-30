package at.hazm.nlp.sample.mallet.lda

import java.io._

import cc.mallet.pipe.iterator.StringArrayIterator
import cc.mallet.pipe.{Input2CharSequence, SerialPipes, TokenSequence2FeatureSequence}
import cc.mallet.topics.ParallelTopicModel
import cc.mallet.types.InstanceList

import scala.collection.JavaConverters._
import scala.io.Source
import scala.util.Random

object AccuracyEvaluation {
  val TrainDataRate = 0.9

  // 学習用のインスタンス (文書) とパイプ (変換処理) を作成
  val pipes = new SerialPipes(Seq(
    new Input2CharSequence("UTF-8"), // ファイルからの文書読み込み想定
    new Japanese2TokenSequence(), // 自前の形態素解析によるトークン化
    new TokenSequence2FeatureSequence() // トークンを ID にマッピング (withBigrams 版もある)
  ).asJava)

  def main(args:Array[String]):Unit = {
    val predictIterations = 10
    val data = extract()
    val model = train(data.values.flatMap(_._1).toSeq, data.size, Double.NaN, 0.1, 5000)
    model.printTopWords(System.out, 20, false)
    System.out.println("----")
    data.toSeq.sortBy(_._1).foreach{ case (name, (train, _)) =>
      val appearances = predictTopic(model, train, predictIterations)
      val (count, topic) = appearances.zipWithIndex.maxBy(_._1)
      System.out.println(f"$name%-15s = Topic $topic%d (${count*100.0/appearances.sum}%.1f%%)")
    }
    System.out.println("----")
    data.toSeq.sortBy(_._1).foreach{ case (name, (_, predict)) =>
      val appearances = predictTopic(model, predict, predictIterations)
      val (count, topic) = appearances.zipWithIndex.maxBy(_._1)
      System.out.println(f"$name%-15s = Topic $topic%d (${count*100.0/appearances.sum}%.1f%%)")
    }
  }

  private[this] def train(docs:Seq[String], topics:Int, alpha:Double, beta:Double, iter:Int, threads:Int = 1):ParallelTopicModel = {
    val file = new File(s"model_t$topics-a$alpha-b$beta-i$iter")
    val model = if(!file.isFile) {

      val instances = new InstanceList(pipes)
      instances.addThruPipe(new StringArrayIterator(docs.toArray))

      // 学習の実行
      val t0 = System.currentTimeMillis()
      println(f"training: Topics=$topics%,d, Σα_t=$alpha%.2f, β=$beta%.2f, iter=$iter%,d")
      val model = new ParallelTopicModel(
        topics, if(alpha.isNaN || alpha < 0.0) topics else alpha, beta)
      model.setNumIterations(iter)
      model.setNumThreads(threads)
      model.addInstances(instances)
      model.estimate()
      val t1 = System.currentTimeMillis()
      println(f"${t1 - t0}%,dms training finish")
      model.write(file)
      model
    } else {
      ParallelTopicModel.read(file)
    }
    model
  }

  private[this] def predictTopic(model:ParallelTopicModel, docs:Seq[String], iter:Int):Seq[Int] = {
    docs.map(doc => predict(model, doc, iter)).foldLeft(new Array[Int](model.numTopics)) { case (topics, probabilities) =>
      topics(probabilities.zipWithIndex.maxBy(_._1)._2) += 1
      topics
    }
  }

  private[this] def predict(model:ParallelTopicModel, doc:String, iter:Int):Array[Double] = {
    val instances = new InstanceList(pipes)
    instances.addThruPipe(new StringArrayIterator(Array(doc)))
    model.getInferencer.getSampledDistribution(instances.get(0), iter, 1, 5)
  }

  private[this] def extract():Map[String, (Seq[String], Seq[String])] = {
    val random = new Random(123456)
    new File(".").listFiles().filter(_.getName.matches("[0-9]{2}_.*\\.tsv")).map { file =>
      val topicId = file.getName
      val docs = random.shuffle(Source.fromFile(file, "UTF-8").getLines().take(1000).map { line =>
        val _ :: _ :: content :: Nil = line.split("\t").toList
        content
      }.toList)
      val trainCount = (docs.length * TrainDataRate).toInt
      System.out.println(f"${file.getName} -> train=$trainCount%,d, predict=${docs.length-trainCount}%,d")
      topicId -> (docs.take(trainCount), docs.drop(trainCount))
    }.toMap
  }

}
