package at.hazm.nlp.sample.mallet.lda

import java.io.StringReader
import java.text.Normalizer

import cc.mallet.extract.StringTokenization
import cc.mallet.pipe.Pipe
import cc.mallet.types.Instance
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.codelibs.neologd.ipadic.lucene.analysis.ja.JapaneseTokenizer
import org.codelibs.neologd.ipadic.lucene.analysis.ja.tokenattributes.{BaseFormAttribute, PartOfSpeechAttribute}

import scala.collection.mutable
import scala.util.Try


/**
  * 日本語の形態素解析 Kuromoji + NEologd を使用した Pipe。
  */
class Japanese2TokenSequence() extends Pipe with Serializable {
  override def pipe(carrier:Instance):Instance = {
    val text = carrier.getData.asInstanceOf[CharSequence].toString
    val ts = tokenize(text).foldLeft(new StringTokenization(text)) { case (tokens, token) =>
      tokens.add(token)
      tokens
    }
    carrier.setData(ts)
    carrier
  }

  /**
    * 指定された文字列を Kuromoji + NEologd で形態素解析します。
    *
    * @param text 形態素解析する文字列
    * @return 処理結果
    */
  private[this] def tokenize(text:String):Seq[String] = {
    type 単語 = String
    type 品詞 = String

    // 半角/全角と大文字/小文字の統一
    def normalize(text:String):String = Normalizer.normalize(text, Normalizer.Form.NFKC).toUpperCase

    val tokenizer = new JapaneseTokenizer(null, true, JapaneseTokenizer.Mode.NORMAL)
    tokenizer.setReader(new StringReader(normalize(text)))
    tokenizer.reset()

    // 形態素に分解
    val buffer = mutable.Buffer[(単語, 品詞)]()
    while(tokenizer.incrementToken()) {
      val term = Option(tokenizer.getAttribute(classOf[BaseFormAttribute]).getBaseForm).getOrElse(
        tokenizer.getAttribute(classOf[CharTermAttribute]).toString
      )
      val pos = tokenizer.getAttribute(classOf[PartOfSpeechAttribute]).getPartOfSpeech.split("-").head
      buffer.append((term, pos))
    }
    tokenizer.close()

    // 前の単語の否定を連結
    buffer.indices.flatMap { i =>
      if(i + 1 == buffer.length) Some(buffer(i)._1) else (buffer(i), buffer(i + 1)) match {
        case ((term, _), _) if Try(term.toInt).isSuccess => None
        case ((term, "形容詞" | "動詞"), ("ない" | "なかっ", _)) => Some(s"${term}ない")
        case (("ない" | "なかっ", _), _) => None
        case ((term, "名詞" | "動詞" | "形容詞"), _) => Some(term)
        case _ => None
      }
    }.toList
  }
}
