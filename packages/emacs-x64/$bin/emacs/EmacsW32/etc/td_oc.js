//	Copyright 2000, LB Information, Lennart Borgman. All rights reserved.

function td_tb_c(title,href,show) {
    var attr_td = new Array();
    attr_td[0]  = 'class="tb"';
    attr_td[1]  = 'title="'+title+'"';
    var attr_a  = new Array();
    attr_a[0]   = 'href="'+href+'"';
    var this_td =
	td(	attr_td.join(" "),
		a(attr_a.join(" "), show)
		);
    return this_td;
}
function td_tb_o(title,href,show) {
    var attr_td = new Array();
    attr_td[0] = 'class="tbo"';
    attr_td[1] = 'title="'+title+' (This is where you are)"';
    var attr_a  = new Array();
    attr_a[0]   = 'class="tba_o"';
    var this_td =
	td(	attr_td.join(" "),
		span(attr_a.join(" "), show)
		);
    return this_td;
}
function td_tb(opened,title,href,show) {
    if (opened) {
	return td_tb_o(title,href,show);
    } else {
	return td_tb_c(title,href,show);
    }
}
