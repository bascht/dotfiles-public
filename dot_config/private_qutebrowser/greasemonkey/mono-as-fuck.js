// ==UserScript==
// @name        MonoAsFuck
// @namespace   https://github.com/bascht
// @description Adjust a few non-mono pages that I like to read
// @match       http://68k.news/*
// @match       https://unfeeder.com/*
// @run-at      document-start
// @version     1
// @author      Bascht
// ==/UserScript==

(function IIFE() {
    'use strict';

    document.addEventListener('readystatechange', function onReadyStateChange() {
            document.body.classList.add("mono-as-fuck");
	});
})();
