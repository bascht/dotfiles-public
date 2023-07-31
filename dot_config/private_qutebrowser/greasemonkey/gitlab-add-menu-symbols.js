// ==UserScript==
// @name        GitLabMenuSymbols
// @namespace   https://github.com/bascht
// @description Adjust a few non-mono pages that I like to read
// @include     /^https:\/\/git.*$/
// @include     https://git.*
// @include     https://gitlab.com*
// @version     1
// @author      Bascht
// @grant       GM_xmlhttpRequest
// @grant       GM.xmlHttpRequest
// ==/UserScript==

(async function IIFE() {
  'use strict';

  const menuMap = {
    "Issues": "🗎",
    "Pipelines": "🚀",
    "Merge requests": "↩️",
    "Epics": "📕",
    "Terraform states": "🇹"
  };
  document.addEventListener('readystatechange', async function onReadyStateChange() {

    var lastTopBarItem = document.querySelector("div.top-bar-container nav ul li:last-child");
    console.log(lastTopBarItem);
    lastTopBarItem.style.marginRight = "20px"
    document.querySelectorAll("ul[data-testid=pinned-nav-items]>li>a").forEach((a) => {
      a.querySelector("div.gl-truncate-end").innerText = [
        menuMap[a.dataset.qaSubmenuItem],
        a.querySelector("div.gl-truncate-end").innerText
      ].join(" ")

      var newTopBarItem = a.cloneNode(true);
      newTopBarItem.removeChild(newTopBarItem.querySelector("div[data-testid=active-indicator]"))
      newTopBarItem.removeChild(newTopBarItem.querySelector("span>button").parentNode)
      newTopBarItem.removeChild(newTopBarItem.querySelector("svg[data-testid=grip-icon]").parentNode)
      lastTopBarItem.parentNode.append(newTopBarItem);
    })
  });
})();
