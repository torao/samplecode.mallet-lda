# Sample Code for Mallet LDA (ParallelTopicModel)

[Mallet](http://mallet.cs.umass.edu/) の gibbs-sampling LDA を使用してドキュメントのトピック分類を行うサンプルコード。

1. 学習用とトピック推定用に使用するデータは1行に1ドキュメントが保存されているテキストファイル。
2. 日本語の形態素解析には [Kuromoji](https://www.atilika.com/ja/products/kuromoji.html) + [NEologd](https://github.com/neologd/mecab-ipadic-neologd) を使用している。

```
$ sbt "runMain train.txt --predict predict.txt"
```

Maven に登録されているライブラリを使用しているが、Mallet のサイトからコマンドライン版がダウンロードできる。
コマンドラインで使用するには [Getting Started with Topic Modeling and MALLET](https://programminghistorian.org/lessons/topic-modeling-and-mallet) のチュートリアルを参照。
