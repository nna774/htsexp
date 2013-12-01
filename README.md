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


# じゃあなんでBuakko に手を加えなかったの

私のperl 力不足です. 本当にありがとうございました.


