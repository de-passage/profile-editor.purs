const marked = require("marked");

exports.markedByElementId = function (elname) {
    return function (toRender) {
        return function () {
            let d = document.getElementById(elname);
            if (d !== null && typeof (d) !== 'undefined') {
                d.innerHTML = marked(toRender, { smartypants: true, silent: true })
            };
        };
    };
};

exports.marked = function (string) {
    return function() {
        return marked(string, { smartypants: true, silent: true });
    };
};
exports.setHTML = function (el) {
    return function (html) {
        return function () {
            el.innerHTML = html;
        };
    };
};