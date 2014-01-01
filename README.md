# HtSExp

S式からHTML に変換するためのものです.

# Buakko あるじゃん

Html を手で書くのはつらいなぁ…… S式のよう自然に書けないのかなぁ…… と思ってたところBuakko ( https://github.com/taiju/Buakko ) を見つけ, これはいいと思って使っていた所, 幾つかの不満点を見つけ, 自分用に修正しようと思ったのですが, Perl で書かれていた為にHaskell で1から書いてみたのがこのHtSExp です.

Buakko 

    <img src="hoge.png" width="42"></img>

のようなものを表現するために

    (img (@src "hoge.png" @width "42"))

と書かないといけないのですが, S式としてはやはりこのコードは

    (script (@type "text/javascript") (@src "hoge.js"))

のように書くのが自然だと私は思います.
これがほぼ唯一の不満でした.

# install

    $ ghc -o htsexp main.hs -O3

をやったあとにできた htsexp をパスの通ったところに置いてください.


# 使い方

    $ htsexp hoge.htsxep > hoge.html

### input file: hoge
```scheme
(html (@lang "ja")
  (head (* ヘッダ *)
    (meta (@charset "utf-8"))
    (title "EXAMPLE"))
  (body (@class "example home") (* ボディ *)
    (h1 "example!")
    (p 
      (a (@href "http://example.com/") (@target "_blank") "example web site."))))
```

### output file: hoge.html
```html
<!doctype html><html lang="ja"><head><!--ヘッダ--><meta charset="utf-8" /><title>EXAMPLE</title></head><body class="example home"><!--ボディ--><h1>example!</h1><p><a href="http://example.com/" target="_blank">example web site.</a></p></body></html>
```

# 書き方
Buakko のほうの例をそのまま使わせていただいています.

## 要素(値なし)
```scheme
(element)
```

```html
<element />
```

## 要素(値あり)
```scheme
(element "value")
```

```html
<element>value</element>
```

## 要素のネスト
```scheme
(element1
  "value1"
  (element2 "value2")
  (element3
    "value3"
    (element3_5 "value3_5"))
  (element4 "value4"))
```

```html
<element1>value1<element2>value2</element2><element3>value3<element3_5>value3_5</element3_5></element3><element4>value4</element4></element1>
```

## 属性
```scheme
(element (@attr "hoge"))
```

```html
<element attr="hoge" />
```

## 属性(複数)
```scheme
(element (@attr1 "hoge") (@attr2 "fuga"))
```

```html
<element attr1="hoge" attr2="fuga" />
```

属性も内部的には要素と同じように扱ってます. 複数並べてください.

## コメント
```scheme
(div (@id "header") (* ヘッダー開始 *)
  (h1 "title")
  (p "Hello!!") (* ヘッダー終了 *)
  )
```

```html
<div id="header"><!--ヘッダー開始--><h1>title</h1><p>Hello!!</p><!--ヘッダー終了--></div>
```

一行コメントは現在対応していません.


## doctype
```scheme
(html (@lang "ja")
  (head
    (meta (@charset "utf-8"))
    (title "title"))
  (body "hello"))
```

```html
<!doctype html><html lang="ja"><head><meta charset="utf-8" /><title>title</title></head><body>hello</body></html>
```

html要素を使うと自動的にdoctypeが付与されます.

# LICENSE

Copyright (C) 2013  NoNameA774@nnn77 <nonamea7.7.4@gmail.com>

GNU General Public License version 3 or any later version.
