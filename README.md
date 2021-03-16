# profile-editor.purs
Editor for my personal website

[Live here](https://sylvainleclercq.com/profile-editor.purs/)

Probably not of interest to anybody but me. It loads JSON files containing entries of the following form:

``` js
{
  "key": {
    "en": "Text in English",
    "fr": "Texte en français",
    "ja": "日本語の文章"
  }
}
```

then display them in 3 different sections with markdown previews, allows for edition then conversion back in JSON. Useful to translate my personal website [sylvainleclercq](https://sylvainleclercq.com).

The code is under MIT license, feel free to reuse it if you have similar needs. Only 3 languages supported, but it shouldn't be to hard to add/remove some others.
