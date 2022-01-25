// ==UserScript==
// @name        FixGitLabPipelines
// @namespace   https://git.bascht.space
// @description Fix display of downstream triggers and job dependencies
// @include     /^https://git.*/pipelines//
// @run-at      document-start
// @version     1
// @author      Bascht
// ==/UserScript==

(function IIFE() {
    'use strict';

    document.addEventListener('DOMContentLoaded', function() {
        const jsTabPipeline = document.querySelector("#js-tab-pipeline div");
        const observer = new MutationObserver(function() {
            // Pick that little wiggly button once the rest of the tab show has stopped it's irish tap dance.
            var expanderButton = document.querySelector('button[aria-label="Expand pipeline"]')

            // Check that there is actually something to expand
            var isExpandable = expanderButton.querySelector("svg[data-testid='angle-right-icon']") !== null

            // Expand if possible
            if(isExpandable) {
                expanderButton.click();
            }
            // Otherwise visualise the job dependencies
            else {
                var linksToggle = document.querySelector('div[data-testid="show-links-toggle"] button[role="switch"]')

                // If it's not yet toggled
                if(linksToggle.ariaChecked == "false") {
                    console.log("Toggle Button")
                    linksToggle.click();
                }
            }
        });
        observer.observe(jsTabPipeline, {subtree: true, childList: true});
    });
})();
