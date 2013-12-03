# これなん？

S式からHTML に変換するためのものです.

# Buakko あるじゃん

Buakko( https://github.com/taiju/Buakko ) と同じような働きをするのですが, Buakko は

    <script type="text/javascript" src="hoge.js"></script>

のようなものを表現するために

    (script (@type "text/javascript" @src "hoge.js"))

と書かないといけない.

という部分が気に入らなかったので, parsec の練習も兼ねて作ってみました.

本当ならばさっき上げたコードは,

    (script (@type "text/javascript") (@src "hoge.js"))

のように書くのがS式としては自然だと私は思います.

# 詳しい説明

大体Buakko と同じになるようにしてあります.

まだXML には対応してないです

# なぜBuakko に手を加えなかったのか

人の書いたPerl 読むの辛いです.

あとparsec って気になってたし一回使ってみたかった.

