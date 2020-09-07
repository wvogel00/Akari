# Akari

## twitterクライアント　AkariApp

twitter Developer用ページでアプリケーションを登録，
oauth文字列とcredential文字列を取得

### 呟く
```Tweet```型は，
- ```text :: text```
- ```img :: Maybe text```

をフィールドとしてもつ．Tweet型の値をtweet関数に渡せば呟いてくれる．

### TL取得
今後追加予定

### スケジュール機能
現在は埋め込みのautoTweet関数だが，外部ファイルを読み込むことで呟く時間・間隔・条件・内容をスケジューリングできるようにする．
外部ファイルにはymlを用いる．書式は以下の通り．

```
tweet:
  - from : 2020-8-31/9:00
    to   : 2020-9-1
    stopfrom : 21:00
    stopto   : 6:00
    span     : 3h
    contents : てすとてすと

  - from : 2020-9-2/10:00
    to   : 2020-9-3/21:00
    stopfrom : 21:00
    stopto   : 7:00
    span     : 3h
    contents : 今日はいい天気だなあ
    media    : image/いい天気.png
```

### via
現在はAkariAppを通じて呟いてもviaが表示されない．
今後変更する
