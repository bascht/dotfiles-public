// ==UserScript==
// @name        GitLabBoardEnhancements
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

    const rtf = new Intl.RelativeTimeFormat("en-GB", {
        localeMatcher: "best fit",
        // numeric: "always",
        style: "long",
    });
    var enhanceGitLabCard = async function(card) {
        const full_issue = $(card).find("h4 a").first().attr("href");
        const origin = window.location.origin;
        const project_path = full_issue.split("/-/issues/")[0].replace(origin + "/", "")
        const issue_id = full_issue.split("/-/issues/")[1]
        const issue_path = origin + "/api/v4/projects/" + encodeURIComponent(project_path) + "/issues/" + issue_id;
        const related_merge_requests_path = origin + "/api/v4/projects/" + encodeURIComponent(project_path) + "/issues/" + issue_id + "/related_merge_requests?per_page=100";
        const resource_state_events_path = origin + "/api/v4/projects/" + encodeURIComponent(project_path) + "/issues/" + issue_id + "/resource_state_events";

        const board_info_items = $(card).find("span.board-info-items").first()

        GM.xmlHttpRequest({
            method: "GET",
            url: issue_path,
            onload: function(response) {
                const issue = JSON.parse(response.responseText);
                const issue_iid = issue.iid; // const updated_days_ago = Math.round((Date.parse(issue.updated_at) - new Date()) / 86400000);
                const project_id = issue.project_id; // const updated_days_ago = Math.round((Date.parse(issue.updated_at) - new Date()) / 86400000);

                GM.xmlHttpRequest({
                    method: "GET",
                    url: origin + "/api/v4/projects/" + issue.project_id + "/issues/" + issue.iid + "/resource_state_events",
                    onload: function(response) {
                        const resource_state_events = JSON.parse(response.responseText); // const updated_days_ago = Math.round((Date.parse(issue.updated_at) - new Date()) / 86400000);
			if(resource_state_events.length > 0) {
                        const last_event = resource_state_events[resource_state_events.lastIndex];
                        const event_timeline = resource_state_events.map(state => "" + state.state).join(" → ");

                          const time_since_last_event = Math.round((Date.parse(last_event.created_at) - new Date()) / 86400000);
                          const days = time_since_last_event != 0 ? rtf.format(time_since_last_event, "day") : "today";

                          const text = last_event.state + " " + days + " by " + last_event.user.username;
                          const div = $("<div>", { class: "last-event gl-display-flex align-items-start", style: "margin-top: 0.75em; margin-bottom: 0.5em;" })
                          const item = $("<div>", { style: "display: flex; align-items: center; background-color: #f2f7fb; width: 100%; border-radius: 3px; padding: 0.1em", title: event_timeline })
                          item.append($("<span>", {style: "display: block; min-width: fit-content; font-weight: lighter; font-size:0.6em"}).append(text));
                          $(div).append(item);
                          $(board_info_items).append(div);
			};

                    }});
		}});
        GM.xmlHttpRequest({
            method: "GET",
            url: related_merge_requests_path,
            onload: function(response) {
                $(board_info_items).css("width", "100%");
                $(board_info_items).find("div.related-merge-requests").each(function(){ $(this).remove()});
                $(JSON.parse(response.responseText)).each(function(){
                    const div = $("<div>", { class: "related-merge-requests gl-display-flex align-items-start", style: "margin-bottom: 0.5em; margin-bottom: 0.5em;" })
                    const item = $("<div>", { style: "display: flex; align-items: center; background-color: #d0e9ff; width: 100%; border-radius: 3px; padding: 0.1em", title: "(" + this.state + "/" + this.detailed_merge_status + ")" + this.title })
                    const status_emoji = (function(mr) {
                        switch(mr.state) {
                        case "merged":
                            const days_ago = Math.round((Date.parse(mr.merged_at) - new Date()) / 86400000);

                            const merged_state = "🏁 ";

                            if(days_ago == 0) {
                                return merged_state + "today" ;
                            } else if (days_ago == 1) {
                                return merged_state + "yesterday";
                            }

                            return merged_state + rtf.format(days_ago, "day");

                        case "opened":
                            switch(mr.detailed_merge_status) {
                            case "mergeable":
                                return "👍";
                            case "opened":
                                return "🔧";
                            case "not_approved":
                                return "⌛";
                            default:
                                return mr.state;
                            };
                        };
                    });

                    item.append($("<span>", {style: "display: block; min-width: fit-content; font-size:0.8em"}).append(status_emoji(this)));
                    item.append($("<a>", {href: this.web_url,  style: "display: block; margin-left: auto; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; line-height: 1rem; align-self: end; direction: rtl; text-align: left; padding: 0.1em;"}).append(this.references.full))

                    $.each(this.reviewers, function(index, reviewer) {
                        $(item).prepend($("<a>", { href: reviewer.web_url, class: "gl-link gl-avatar-link user-avatar-link js-no-trigger user-avatar-link" })
                                        .append($("<span>").append($("<img>", { class: "gl-avatar gl-avatar-circle gl-avatar-s16 gl-lg-avatar-s24", src: reviewer.avatar_url} ))))
                    });

                    $(div).append(item)
                    $(board_info_items).prepend(div)

                })
            }
        });
    };

    if (window.location.href.toString().includes('/boards/')) {
        document.addEventListener('readystatechange', async function onReadyStateChange() {
            $("body").on("click", function(e){

                var card = $(e.target).parents("li[data-testid='board-card']").first();
                if(card.length > 0) {
                    enhanceGitLabCard(card)
                }
            });
        });
    };
})();
