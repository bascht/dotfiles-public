// ==UserScript==
// @name        GitLabIssueBoardWiderLanes
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

  if (window.location.href.toString().includes('/boards/')) {
    const targetNode = document.getElementById("content-body");

    // Options for the observer (which mutations to observe)
    const config = { attributes: true, childList: false, subtree: true };

    // Callback function to execute when mutations are observed
    const callback = (mutationList, observer) => {
      for (const mutation of mutationList) {
        if (mutation.target.classList.contains("board-list")) {
          observer.disconnect();
          document.querySelectorAll("div[data-qa-selector=board_list]").forEach((list) => {
            const plusButton = list.querySelector("button[data-testid=new-issue-btn]");
            if(plusButton){
              const plusButtonClone = plusButton.cloneNode(true);
              plusButtonClone.addEventListener("click", function(e){
                if(list.style.width == "") {
                  list.style.width = "700px";
                } else {
                  list.style.removeProperty("width");
                }
              })
              plusButtonClone.innerHTML = "<>"
              plusButton.parentNode.prepend(plusButtonClone);
            }
          });
        };
      }
    };

    const observer = new MutationObserver(callback);
    observer.observe(targetNode, config);
  };
})();
