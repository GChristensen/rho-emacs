function include(f) { document.write('<script type="text/javascript" src="'+f+'"></script>'); }
include("html.js");
include("td_oc.js");
include("bar.js");
function emacs_onload() {
    document.getElementById('bar').innerHTML = bar_toc(page_num, "");
}
window.onload =  emacs_onload;
