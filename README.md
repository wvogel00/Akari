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
現在は埋め込みのautoTweet関数だが，外部ファイルを読み込むことで
呟く時間・間隔・条件・内容をスケジューリングできるようにする．

### via
現在はAkariAppを通じて呟いてもviaが表示されない．
今後変更する
