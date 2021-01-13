:: 新建chrome标签属性内容
:: 网页捕获
:: javascript:location.href = 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title)

:: 网页内容标注
:: javascript:location.href = 'org-protocol://roam-ref?template=a&ref=' + encodeURIComponent(location.href) + '&title='+encodeURIComponent(document.title) + '&body='+encodeURIComponent(function(){var html = "";var sel = window.getSelection();if (sel.rangeCount) {var container = document.createElement("div");for (var i = 0, len = sel.rangeCount; i < len; ++i) {container.appendChild(sel.getRangeAt(i).cloneContents());}html = container.innerHTML;}var dataDom = document.createElement('div');dataDom.innerHTML = html;['p', 'h1', 'h2', 'h3', 'h4'].forEach(function(tag, idx){dataDom.querySelectorAll(tag).forEach(function(item, index) {var content = item.innerHTML.trim();if (content.length > 0) {item.innerHTML = content + '&#13;&#10;';}});});return dataDom.innerText.trim();}())

@echo off
set URL=%1
set URL=%URL:&=^&%
set URL=%URL:/?=?%
set URL=%URL:://=:///%
start "" "D:\\Tools\\Emacs\\x86_64\\bin\\emacsclientw"  -na "D:\\Tools\\Emacs\\x86_64\\bin\\runemacs" "%URL%"
