// ==UserScript==
// @name        GitLabBoardEnhancements
// @namespace   https://github.com/bascht
// @description Adjust a few non-mono pages that I like to read
// @include     /^https://git.*
// @version     1
// @author      Bascht
// ==/UserScript==

(async function IIFE() {
    'use strict';

    var enhanceGitLabCard = async function(card) {
        const full_issue = $(card).find("h4 a").first().attr("href");
        var project_path = full_issue.split("/-/issues/")[0].replace("https://git.alfa.sx/", "")
        var issue_id = full_issue.split("/-/issues/")[1]
        var full_api_path = "https://git.alfa.sx/api/v4/projects/" + encodeURIComponent(project_path) + "/issues/" + issue_id + "/related_merge_requests?per_page=100";

        const board_info_items = $(card).find("span.board-info-items").first()
        GM.xmlHttpRequest({
            method: "GET",
            url: full_api_path,
            onload: function(response) {
                $(board_info_items).find("div.related-merge-requests").each(function(){ $(this).remove()});
                $(JSON.parse(response.responseText)).each(function(){
                    var div = $("<div>", { class: "related-merge-requests gl-display-flex align-items-start" })
                    var a = $("<a>", { href: this.web_url, title: this.title })
                    a.html("<code>[" + this.state + "]</code>" + this.references.full)
                    $(div).append(a)
                    $(board_info_items).append(div)

                })
            }
        });
    };

    document.addEventListener('readystatechange', async function onReadyStateChange() {
        $("body").on("click", function(e){

            var card = $(e.target).parents("li[data-qa-selector='board_card']").first();
            if(card) {
                enhanceGitLabCard(card)
            }
        });
    });
})();
