/////////////
// HTML
/////////////
var nl = "";
function tag(name,attr,val)	{
	var  		tag = '<'+name;
	if (attr) 	tag += ' '+attr;
	if (typeof val != "undefined") {
				tag += '>'+val+'</'+name+'>'; }
	    else {  tag += ' />';             }
	return tag+nl;
}
function script(attr,val)	{ return(tag('script','type="text/javascript" '+attr, val)); }
function html(val)			{ return(tag('html',  '',  val));	}
function body(attr,val)		{ return(tag('body',  attr,val));	}
function head(attr,val)		{ return(tag('head',  attr,val));	}
function title(val)			{ return(tag('title', '',  val));	}
function span(attr,val)		{ return(tag('span',  attr,val));	}
function table(attr,val)	{ return(tag('table', attr,val)); 	}
function tr(attr,val)		{ return(tag('tr',    attr,val)); 	}
function td(attr,val)		{ return(tag('td',    attr,val)); 	}
function img(attr)			{ return(tag('img',   attr));		}
function hr(attr)			{ return(tag('hr',    attr));		}
function br(attr)			{ return(tag('br',    attr));		}
function p(attr,val)		{ return(tag('p',     attr,val));	}
function a(attr,val)		{ return(tag('a',     attr,val));	}
function form(attr,val)		{ return(tag('form',  attr,val));	}
function input(attr)		{ return(tag('input', attr));		}
function iframe(attr,val)	{ return(tag('iframe',attr,val));	}
function div(attr,val)		{ return(tag('div',   attr,val));	}
function nbsp(i)			{ var r=""; do { r=r+"&nbsp;"; } while (i--); return r; }

function ol(attr,val)		{ return(tag('ol',   attr,val));	}
function li(attr,val)		{ return(tag('li',   attr,val));	}

function b(attr,val)		{ return(tag('b',    attr, val));	}
function pre(attr,val)		{ return(tag('pre',  attr, val));	}

function stylelink(href) {
	return tag('link', ' rel="stylesheet" type="text/css" href="'+href+'"');
}
function basehref(href) {
	return tag('base', ' href="'+href+'"' );
}
function comment(txt) {
	return "<!-- "+txt+" -->";
}
