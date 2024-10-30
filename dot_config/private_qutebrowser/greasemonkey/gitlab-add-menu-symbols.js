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
        "Terraform states": "🦖"
    };
    document.addEventListener('readystatechange', async function onReadyStateChange() {

        var lastTopBarItem = document.querySelector("div.top-bar-container nav ol li:last-child");
        if (lastTopBarItem) {
            lastTopBarItem.style.marginRight = "20px"
            document.querySelectorAll("ul[data-testid=pinned-nav-items]>li>a").forEach((a) => {
                if(typeof(a) == "object") {
                    a.querySelector("div.gl-flex").innerText = [
                        menuMap[a.dataset.qaSubmenuItem],
                        a.querySelector("div.gl-grow").innerText
                    ].join(" ")

                    var newTopBarItem = a.cloneNode(true);
                    newTopBarItem.style.fontSize = "0.8em";
                    newTopBarItem.removeChild(newTopBarItem.querySelector("div[data-testid=active-indicator]"))

                    if(newTopBarItem.querySelector("span.badge-pill") !== null) {
	                      const badgeCount = Number(newTopBarItem.querySelector("span.badge-pill")?.innerText.trim())
	                      if(badgeCount == 0 || isNaN(badgeCount)) {
		                        newTopBarItem.removeChild(newTopBarItem.querySelector("span.badge-pill").parentNode)
	                      }
                    }

                    const glTextRight = newTopBarItem.querySelector(".gl-text-right")?.innerText.trim();
                    if(glTextRight == undefined || glTextRight == "") {
                        if(newTopBarItem.querySelector(".gl-text-right") != null){
                            newTopBarItem.removeChild(newTopBarItem.querySelector(".gl-text-right"))
                        }
                    }

                    // The new button in the top bar
                    const newButton = newTopBarItem.querySelector("div.gl-w-6")
                    // The badge / pill (only happens in MR / issues)
                    const badgeButton  = newButton.parentElement.querySelector("span")

                    // The buttons need more room the breathe
                    newButton.classList.remove("gl-w-6")
                    newButton.classList.add("gl-w-16")
                    newButton.parentElement.classList.add("gl-min-w-fit")

                    // Remove empty pills (Terraform, etc)
                    if (badgeButton?.children?.length == 0) {
                        badgeButton.remove()
                    }

                    // Remove the copied item and it's ancestor
                    newTopBarItem.removeChild(newTopBarItem.querySelector("div.gl-grow"))
                    lastTopBarItem.parentNode.append(newTopBarItem);
                    a.querySelector("div.gl-w-6").remove()


                };
            })
        }}
    );
})();
