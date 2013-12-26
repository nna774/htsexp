# HtSExp

S������HTML ���Ѵ����뤿��Τ�ΤǤ�.

# Buakko ���뤸���

Html ���ǽ񤯤ΤϤĤ餤�ʤ��ġ� S���Τ褦�����˽񤱤ʤ��Τ��ʤ��ġ� �ȻפäƤ��Ȥ���Buakko ( https://github.com/taiju/Buakko ) �򸫤Ĥ�, ����Ϥ����ȻפäƻȤäƤ�����, ���Ĥ����������򸫤Ĥ�, ��ʬ�Ѥ˽������褦�Ȼפä��ΤǤ���, Perl �ǽ񤫤�Ƥ����٤�Haskell ��1����񤤤Ƥߤ��Τ�����HtSExp �Ǥ�.

Buakko 

    <img src="hoge.png" width="42"></img>

�Τ褦�ʤ�Τ�ɽ�����뤿���

    (img (@src "hoge.png" @width "42"))

�Ƚ񤫤ʤ��Ȥ����ʤ��ΤǤ���, S���Ȥ��ƤϤ�Ϥꤳ�Υ����ɤ�

    (script (@type "text/javascript") (@src "hoge.js"))

�Τ褦�˽񤯤Τ��������Ȼ�ϻפ��ޤ�.
���줬�ۤ�ͣ��������Ǥ���.

# install

    $ ghc -o htsexp main.hs -O3

���ä����ȤˤǤ��� htsexp ��ѥ����̤ä��Ȥ�����֤��Ƥ�������.


# �Ȥ���

    $ htsexp hoge.htsxep > hoge.html

### input file: hoge
```scheme
(html (@lang "ja")
  (head (* �إå� *)
    (meta (@charset "utf-8"))
    (title "EXAMPLE"))
  (body (@class "example home") (* �ܥǥ� *)
    (h1 "example!")
    (p 
      (a (@href "http://example.com/") (@target "_blank") "example web site."))))
```

### output file: hoge.html
```html
<!doctype html><html lang="ja"><head><!--�إå�--><meta charset="utf-8" /><title>EXAMPLE</title></head><body class="example home"><!--�ܥǥ�--><h1>example!</h1><p><a href="http://example.com/" target="_blank">example web site.</a></p></body></html>
```

# ����
Buakko �Τۤ�����򤽤Τޤ޻Ȥ碌�Ƥ��������Ƥ��ޤ�.

## ����(�ͤʤ�)
```scheme
(element)
```

```html
<element />
```

## ����(�ͤ���)
```scheme
(element "value")
```

```html
<element>value</element>
```

## ���ǤΥͥ���
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

## °��
```scheme
(element (@attr "hoge"))
```

```html
<element attr="hoge" />
```

## °��(ʣ��)
```scheme
(element (@attr1 "hoge") (@attr2 "fuga"))
```

```html
<element attr1="hoge" attr2="fuga" />
```

°��������Ū�ˤ����Ǥ�Ʊ���褦�˰��äƤޤ�. ʣ���¤٤Ƥ�������.

## ������
```scheme
(div (@id "header") (* �إå������� *)
  (h1 "title")
  (p "Hello!!") (* �إå�����λ *)
  )
```

```html
<div id="header"><!--�إå�������--><h1>title</h1><p>Hello!!</p><!--�إå�����λ--></div>
```

��ԥ����Ȥϸ����б����Ƥ��ޤ���.


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

html���Ǥ�Ȥ��ȼ�ưŪ��doctype����Ϳ����ޤ�.

# LICENSE

Copyright (C) 2013  NoNameA774@nnn77 <nonamea7.7.4@gmail.com>

GNU General Public License version 3 or any later version.
