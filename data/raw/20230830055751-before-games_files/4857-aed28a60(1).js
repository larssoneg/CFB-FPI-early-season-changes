(self.webpackChunkfitt=self.webpackChunkfitt||[]).push([[4857],{699728:function(e,t,r){var o=r(785893),l=(r(667294),r(45697)),s=r(400421);function a(e){return o.jsx("span",{className:"dib arrow-icon_cont",style:{width:"10px",height:"10px"},children:o.jsx(s,{className:"w-70",icon:`caret__${e.type}`})})}a.propTypes={type:l.string.isRequired},a.defaultProps={type:"down"},e.exports=a},412343:function(e,t,r){var o,l=r(785893),s=(r(667294),r(450239)),a=s.first,i=s.forEach,n=s.isFunc,c=r(605902),d=r(227361),h=r(944908),f=r(606162),u=r(838169),p=r(903896),w=r(45697),b=r(834248).PropTypes,g=r(294184),m=r(833439).THEMES.DARK,S=r(684967),T=S.qsa,x=S.rect,y=S.scrollTo,R=r(740804),_=r(289173);const A="breakpoint:className:update";function L(e,t,r){let o,l,s,i,n,c,d=T(".ResponsiveTable .Table__Scroller",!0,e);if(d&&t)try{o=x(d),l=T(".Table__Scroller > .Table",!0,d),s=x(l),i=a(l.rows),n=i.cells[t-1],c=x(n),(c.l<o.l||c.r>o.r)&&y(c.l-s.l,0,d)}catch(e){}}function v(){var e,t,r=this,o=r.props;(o.fixedLeftRows||o.fixedRightRows)&&(e=T(".Table__Scroller > .Table",!0,r.respTable))&&(t=T(".Table--fixed.Table",!1,r.respTable),i(e.rows,((e,r)=>{let o,l,s=[x(e).h];i(t,((e,t)=>{let o,l,a=e.rows,i=a[r];if(i){try{i.style.height="auto"}catch(e){}o=x(a[r]),l=o&&o.h,s.push(l)}})),l=h(s),l.length>1&&(o=f(l),i(t,((e,t)=>{const l=e.rows[r];if(l)try{l.style.height=o+"px"}catch(e){}})))})))}(o=p({displayName:"ResponsiveTable",getDefaultProps:function(){return{className:"",rows:[],headings:[],caption:null,footer:!1,fixedLeftData:[],fixedLeftHeadings:[],fixedLeftCaption:null,fixedRightData:[],fixedRightHeadings:[],fixedRightCaption:null,showScroll:!0,rowHeight:"sm",theme:"",manualRowRender:!1,uniformWidths:!1}},propTypes:{className:w.string,rows:w.oneOfType([w.array,b.observableArray]),headings:w.oneOfType([w.object,w.arrayOf(w.oneOfType([w.array,w.object,w.string]))]),footer:w.bool,caption:w.string,fixedLeftRows:w.arrayOf(w.oneOfType([w.array,w.object])),fixedLeftHeadings:w.array,fixedRightRows:w.arrayOf(w.oneOfType([w.array,w.object])),fixedRightHeadings:w.array,rowHeight:w.oneOf(["sm","md","lg"]),colgroups:w.oneOfType([b.observableArray,w.array]),fixedLeftColgroups:w.array,fixedRightColgroups:w.array,multilineColumns:w.array,onScroll:w.func,fixedLeftMultilineColumns:w.array,fixedRightMultilineColumns:w.array,borderRow:w.number,sortedCol:w.number,showScroll:w.bool,arrowScroll:w.bool,shouldScroll:w.bool,arrowOffsets:w.object,theme:w.string,manualRowRender:w.bool,uniformWidths:w.bool,mobileShadowConfig:w.bool},componentDidMount:function(){let e=this,t=e.props,r=t.shouldScroll,o=t.sortedCol,l=t.subheaders,s=e.respTable;v.call(e),r&&(e.boundScroll=u(L,null,s,o,l.length>0),c.on(A,e.boundScroll))},componentDidUpdate:function(e,t){this.updateEmitter(e),this.refreshTable()},refreshTable:function(){let e=this.props.shouldScroll;v.call(this),e&&c.emit(A)},updateEmitter:function(e){let t=this,r=t.props,o=e.sortedCol,l=r.sortedCol,a=t.respTable,i=r.subheaders||r.headings,n=s.isAry(i)?i.length:0;o!==l&&(c.off(A,t.boundScroll),t.boundScroll=u(L,null,a,l,n>0),c.on(A,t.boundScroll))},setRef:function(e){const t=this,r=t&&t.setState,o=d(t,"props.setTableRef");r&&(e?(t.respTable=e,n(o)&&o(e)):delete t.respTable)},render:function(){var e,t=this,r=t.setRef,o=t.props,s=t.context,a=s&&s.theme||"",i=o.className,n=o.rows,c=o.headings,d=o.title,h=o.caption,f=o.description,u=o.footer||!1,p=o.fixedLeftRows,w=o.fixedLeftHeadings,b=o.fixedLeftSubheaders,S=o.fixedLeftCaption,T=o.fixedLeftDescription,x=o.fixedRightRows,y=o.fixedRightHeadings,A=o.fixedRightSubheaders,L=o.fixedRightCaption,v=o.fixedRightDescription,N=o.sortedCol&&o.sortedCol,C=o.borderRow,O=o.rowHeight,j=o.textAlign||"right",H=o.showBorderBottom,E=o.showScroll,M=o.arrowScroll,W=o.arrowOffsets,D=o.shadowArrowResize,k=o.uniformWidths,F=o.manualRowRender,B=o.ariaLabel,q=a===m,P=o.classNameTable,U=o.isFixedScrollArrow,z=o.shouldScrollToRight,$=o.mobileShadowConfig;return i=g("ResponsiveTable",{"ResponsiveTable--has-footer":u,"ResponsiveTable--fixed-left":p,"ResponsiveTable--fixed-right":x,"ResponsiveTable--dark":q},i),e=g("Table__Title",{"Table__Title--dark":q}),l.jsxs("div",{ref:r,id:o.id,className:i,children:[d?l.jsx("div",{className:e,children:d},`${d}-Table`):null,l.jsxs("div",{className:"flex",children:[p&&l.jsx(_,{ariaLabel:B,rowHeight:O,headings:w,colgroups:o.fixedLeftColgroups,multilineColumns:o.fixedLeftMultilineColumns,rows:p,subheaders:b,caption:S,side:"left",borderRow:C,rowSpan:"2",sortedCol:o.sortedLeft,textAlign:j,showBorderBottom:H,oddStripes:o.oddStripes,manualRowRender:F,className:i,uniformWidths:k,description:T,classNameTable:P,fixed:!0}),l.jsx(R,{arrowScroll:M,arrowOffsets:W,isFixedScrollArrow:U,onScroll:o.onScroll,rowHeight:O,shadowArrowResize:D,shouldScrollToRight:z,mobileShadowConfig:$,showScroll:E,children:l.jsx(_,{ariaLabel:B,rowHeight:O,headings:c,colgroups:o.colgroups,multilineColumns:o.multilineColumns,subheaders:o.subheaders,rows:n,caption:h,borderRow:C,sortedCol:N,"align-left":!x&&!p||x,scroll:E,textAlign:j,showBorderBottom:H,oddStripes:o.oddStripes,className:i,manualRowRender:F,uniformWidths:k,description:f,classNameTable:P})}),x&&l.jsx(_,{ariaLabel:B,rowHeight:O,headings:y,colgroups:o.fixedRightColgroups,multilineColumns:o.fixedRightMultilineColumns,subheaders:A,borderRow:C,sortedCol:o.sortedRight,rows:x,caption:L,side:"right",rowSpan:"2",textAlign:j,showBorderBottom:H,oddStripes:o.oddStripes,className:i,manualRowRender:F,uniformWidths:k,description:v,classNameTable:P,fixed:!0})]})]})}})).contextTypes={theme:w.string},e.exports=o},908332:function(e,t,r){var o=r(308742),l=r(603160),s=r(686371).TableHeading,a=r(292522),i=r(289173),n=r(412343),c=r(413661);r(909146),e.exports={FixedTable:a,Table:n,Table__Row:o,Table__Data:l,Table__Heading:s,TableDetails:c,Single__Table:i}},603160:function(e,t,r){var o=r(785893),l=(r(667294),r(294184)),s=r(450239).omit;function a(e){var t=l(e.className,"Table__TD"),r=e&&e.title,a=e&&e.colSpan,i=e&&e.style||{},n=s(e,["className","title","colSpan","children"]);return i.width&&(t=l(t,"Table__TD--fixed-width")),o.jsx("td",{title:r,colSpan:a,className:t,...n,children:e.children})}a.displayName="TableData",e.exports=a},413661:function(e,t,r){var o=r(785893),l=(r(667294),r(294184)),s=r(833439),a=r(45697),i=r(834248).PropTypes,n=s.THEMES.DARK;function c(e,t){var r,s=t&&t.theme,a=e&&e.ariaLabel,i=e&&e.className||"",c=e&&e.data||[],d=e&&e.note||"",h=s===n;return r=d?o.jsx("div",{className:"TableDetails__Note",dangerouslySetInnerHTML:{__html:d}}):c.map((function(e,t){let r=e.description.length?e.headline:null;return o.jsxs("p",{className:"TableDetails__Paragraph",children:[o.jsx("span",{className:"TableDetails__Headline","aria-label":a,children:r})," ",e.description]},t)})),i=l("TableDetails",{"TableDetails--dark":h},i),o.jsx("div",{className:i,children:r})}r(70288),c.contextTypes={theme:a.string},c.propTypes={ariaLabel:a.string,className:a.string,data:a.oneOfType([a.array,i.observableArray]),note:a.string},c.defaultProps={className:"",data:[],note:""},e.exports=c},292522:function(e,t,r){var o=r(667294),l=o.isValidElement,s=o.cloneElement,a=o.createElement,i=r(450239),n=i.isAry,c=i.isObj,d=i.mix,h={owned:!0,noRecurse:!0},f=i.cstr,u=i.omit,p=r(435161),w=r(512571),b=r(252628),g=r(308742),m=r(412343);function S(e){return l(e)?s(e):n(e,!0)?p(e,S):e}e.exports=function(e){let t,r=e.rows||[],o=e.headings||[],s=e.subheaders||[],i=e.caption,T=e.fixedLeftCaption,x=e.fixedRightCaption,y=null,R={};if(r=p(r,S),t=r.length,t){let S,_,A,L,v,N=e.fixedLeft||0,C=e.fixedRight||0,O=e.colgroups||[],j=e.multilineColumns||[],H=e.sortedCol;if(N||C){if(N&&t){for(S=O.length,_=A=0;_<S;_++)if(A+=O[_],N<=A){N=A,R.fixedLeftColgroups=w(O,0,_+1),R.fixedLeftMultilineColumns=w(j,0,_+1),O=w(O,S?_+1:N),j=w(j,_+1);break}R.fixedLeftRows=p(r,(e=>{var t=null;if(l(e)){let r=e.props,o=u(r,["children"]),l=r.children;n(l,!0)&&(l=w(l,0,N)),t=a(g,o,l)}else n(e,!0)?t=w(e,0,N):c(e)&&(t=w(b(e),0,N));return t})),R.fixedLeftHeadings=w(o,0,S?_+1:N),R.fixedLeftSubheaders=w(s&&s,0,N),r=p(r,(e=>{var t=null;if(l(e)){let r=e.props,o=u(r,["children"]),l=r.children;n(l,!0)&&(l=w(l,N)),t=a(g,o,l)}else n(e,!0)?t=w(e,N):c(e)&&(t=w(b(e),N));return t})),o=w(o,_+1),s=w(s&&s,N)}if(C&&r.length){for(S=O.length,_=O.length-1,A=0;_>=0;_--)if(A+=O[_],C<=A){C=A,R.fixedRightColgroups=w(O,_),R.fixedRightMultilineColumns=w(j,_),O=w(O,0,_),j=w(j,0,_);break}R.fixedRightRows=p(r,(e=>{var t=null;if(l(e)){let r=e.props,o=u(r,["children"]),l=r.children;n(l,!0)&&(l=w(l,-C)),t=a(g,o,l)}else n(e,!0)?t=w(e,-C):c(e)&&(t=w(b(e),-C));return t})),R.fixedRightHeadings=w(o,S?_:-C),R.fixedRightSubheaders=w(s&&s,-C),r=p(r,(e=>{var t=null;if(l(e)){let r=e.props,o=u(r,["children"]),l=r.children;n(l,!0)&&(l=w(l,0,-C)),t=a(g,o,l)}else n(e,!0)?t=w(e,0,-C):c(e)&&(t=w(b(e),0,-C));return t})),o=w(o,0,S?_:-C),s=w(s&&s,0,-C)}let E,M=r&&r.length&&r[0];l(M)?(E=M.props.children,L=n(E,!0)?E.length:1):L=M&&M.length||0,v=s.length||o.length||L,H<=N?(R.sortedLeft=H,R.sortedCol=null):H<=N+v?R.sortedCol=H-N:H<=N+v+C&&(R.sortedRight=H-(N+v),R.sortedCol=null),(i||T||x)&&(R.caption=f(i),R.fixedLeftCaption=f(T),R.fixedRightCaption=f(x)),R.rows=r,R.headings=o,R.subheaders=s,R.colgroups=O,R.multilineColumns=j,R=d({},[e,R],h),y=a(m,R)}else y=a(m,e)}return y}},686371:function(e,t,r){var o=r(785893),l=(r(667294),r(294184)),s=r(450239),a=s.omit,i=s.forEach,n=s.emptyObj,c=s.NULL,d=r(58171),h=r(308742),f=r(699728);function u(e){var t=l(e.className,"Table__TH"),r=e&&e.title,s=e&&e.colSpan,i=e&&e.caret,n=i?o.jsx(f,{type:i}):null,c=a(e,["title","className","colSpan","caret","children"]);return o.jsxs("th",{title:r,colSpan:s,className:t,...c,children:[e.children,n]})}function p(e){var t=[];return e?(i(e,((e,r)=>{var o=e&&e.props,l=o&&o.className||e&&e.className,s=e&&e.ttl||o&&o.title||"";null!=e?t.push(d(e.data||e,u,{key:r,className:l||"",title:s})):t.push(d(e,u,{key:r}))})),t):c}u.displayName="TableHeading",e.exports={TableHeading:u,Table__Header:function(e){var t=e.headings,r=t&&p(t)||n,s=l(e.className,"Table__THEAD"),a=c,i=e.subheadings,d=i&&p(i)||n,f=e.subClassName,u=c;return t||i?(r.length&&(a=o.jsx(h,{children:r})),d.length&&(u=o.jsx(h,{className:f,children:d})),o.jsxs("thead",{className:s,children:[a,u]})):c}}},289173:function(e,t,r){var o=r(785893),l=r(667294),s=l.isValidElement,a=r(294184),i=r(45697),n=r(834248).PropTypes,c=r(450239),d=c.mix,h={owned:!0,noRecurse:!0},f=c.forEach,u=l.Children.forEach,p=c.omit,w=r(435161),b=r(84753),g=r(264721),m=r(58171),S=r(686371).Table__Header,T=r(308742),x=r(603160);function y(e){var t=e&&e.ariaLabel,r=e&&e.ariaSummary;return o.jsx("table",{style:{borderCollapse:"collapse",borderSpacing:0},className:a("Table",e.className),"aria-label":t,summary:r,children:e.children})}function R(e){var t=e&&e.ariaLabel,r=e&&e.ariaSummary,l=e.fixed,i=e.rows||[],n=e.headings||[],R=e.rowHeight,_=e.borderRow,A=e.showBorderBottom||!1,L=e.caption,v=e.textAlign||"right",N=i.length&&function(e){return b(e,(e=>e.length||0)).length||0}(i),C=e.colgroups&&e.colgroups.length&&e.colgroups||N>0&&[N]||n.length&&[n.length],O=e.multilineColumns||[],j=e.subheaders,H=e.sortedCol,E=e.uniformWidths,M=e.side,W=a({"Table--align-center":"center"===v,"Table--align-right":"right"===v,"bb brdr-clr-gray-07":A,"Table--fixed-layout":E&&!l},l?`Table--fixed Table--fixed-${M}`:null,e.classNameTable),D=a({"Table__header-group":j}),k=[],F=[],B=0,q=f;if(!0===e.manualRowRender?k=i:f(i,(function(t,r){let o,l=[],a=t,i={rowHeight:R,borderRow:_===B+1,"data-idx":B,oddStripes:e.oddStripes};s(t)&&(o=t.props||{},i=d(i,o,h),i.key=o.id,i=p(i,["children"]),a=o.children,q=u),i.key=i.key||r,q(a,((e,t)=>{var r,o=e&&e.props&&e.props.className||e&&e.className;(r=null!=e?m(e.data||e,x,{key:t,className:o||""}):m(e,x,{key:t}))&&l.push(r)})),k.push(m(l,T,i)),B++})),C&&C.length){let e=0;F=w(C,((t,r)=>{let l=0,s="",i=[],n=O.length&&O[r]||[];for(;l<t;l++)s=a("Table__Column",{"Table--sorted":H===e+1,multiline:g(n,l)}),i.push(o.jsx("col",{className:s},e)),e++;return o.jsx("colgroup",{className:"Table__Colgroup",children:i},e)}))}return o.jsxs(y,{className:W,onScroll:e.onScroll,ariaLabel:t,ariaSummary:r,children:[L!==c.NULL&&o.jsx("caption",{className:"Table__Caption",children:L}),F,o.jsx(S,{headings:n,className:D,subheadings:j,subClassName:"Table__sub-header"}),o.jsx("tbody",{className:"Table__TBODY",children:k})]})}R.propTypes={ariaLabel:i.string,ariaSummary:i.string,className:i.string,classNameTable:i.string,rows:i.oneOfType([i.arrayOf(i.oneOfType([n.observableArray,i.array,i.object,i.element])),n.observableArrayOf(i.oneOfType([n.observableArray,i.array,i.object,i.element]))]).isRequired,headings:i.oneOfType([i.number,i.object,i.arrayOf(i.oneOfType([i.array,i.object,i.string]))]),caption:i.oneOfType([i.element,i.string]),colgroups:i.oneOfType([n.observableArray,i.array]),fixed:i.bool,side:i.string,rowHeight:i.oneOf(["sm","md","lg"]),borderRow:i.oneOfType([i.bool,i.number]),multilineColumns:i.array,sortedCol:i.number,scroll:i.bool,oddStripes:i.bool,manualRowRender:i.bool,uniformWidths:i.bool},R.defaultProps={className:"",rows:[],headings:[],caption:null,rowHeight:"sm",scroll:!1,manualRowRender:!1,uniformWidths:!1},e.exports=R},308742:function(e,t,r){var o=r(785893),l=(r(667294),r(294184)),s=r(450239).omit;function a(e){var t=e.rowHeight,r=e&&e.title,a=t?`Table__TR--${t}`:null,i=e.oddStripes?"Table__odd":"Table__even",n=l(e.className,"Table__TR",a,i,{"Table__border-row":e.borderRow}),c=s(e,["rowHeight","title","className","borderRow","children","oddStripes"]);return o.jsx("tr",{title:r,className:n,...c,children:e.children})}a.displayName="Table__Row",e.exports=a},740804:function(e,t,r){var o,l=r(785893),s=(r(667294),r(450239)),a=r(684967),i=r(903896),n=r(23279),c=r(227361),d=r(777702),h=r(823493),f=r(76902),u=(new(r(406197))).MOBILE[1],p=a.rect,w=s.emptyObj,b=s.isFunc,g=s.TRUE,m=s.FALSE;const S=".Table__Scroller",T={sm:12,md:2,lg:4},x=s.cstr;function y(){var e=this;e.leftOpacity=e.rightOpacity=e.persistLeftScroll=e.scrollableWidth=e.firstScrollColumn=e.navHeight=e.wrapperWidth=0,e.rightArrowAlt=e.leftArrowAlt=g,e.showRightArrow=e.showLeftArrow=m,e.isFixedScrollArrow=m}function R(e){this.leftArrowAlt=e}function _(e){this.rightArrowAlt=e}function A(e){this.opacity=e}function L(e){this.leftOpacity=e}function v(e){this.rightOpacity=e}o=d(o=i({getInitialState:function(){return new y},componentDidMount:function(){var e,t,r,o,l,s,i,n=this,c=n.props||w,d=(a.qsa(".Site__Header",g)||w).offsetHeight||0;n&&n.setState&&(n.handleScroll=h(n.handleScroll,250),n.initElWidth(),n.setState({navHeight:d})),(c.isFixedScrollArrow||c.shouldScrollToRight)&&(i=n._table,l=a.qsa(S,g,i),s=a.qsa(".Table",g,i),r=p(l)||w,o=p(s)||w,t=r.w,e=o.w,c.isFixedScrollArrow&&(e>t&&n.animateArrows(c.isFixedScrollArrow,0,l,{showLeftArrow:m,showRightArrow:g,rightOpacity:1,leftOpacity:0}),n.setState({isFixedScrollArrow:g})),c.shouldScrollToRight&&e>t&&n.animateArrows(c.isFixedScrollArrow,e,l,{showLeftArrow:g,showRightArrow:m,rightOpacity:0,leftOpacity:1}))},debounceScroll:function(e){try{e.persist(),this.handleScroll(e)}catch(e){}},onMouseLeave:function(e){const t=this;t&&t.setState&&(t.setState({showLeftArrow:m}),t.setState({showRightArrow:m}))},onLeftArrowMouseEnter:function(e){const t=this;t&&t.setState&&t.setState(new R(m))},onLeftArrowMouseLeave:function(e){const t=this;t&&t.setState&&t.setState(new R(g))},onRightArrowMouseEnter:function(e){const t=this;t&&t.setState&&t.setState(new _(m))},onRightArrowMouseLeave:function(e){const t=this;t&&t.setState&&t.setState(new _(g))},initElWidth:function(){var e=this||w,t=e._table||w,r=p(t).w,o=t.querySelector(".Table__Scroller > Table"),l=p(o).w,s=e.props||w,a=l>r;e.setState({scrollableWidth:l,wrapperWidth:r}),a?e.setState(new v(1)):(e.setState({showRightArrow:m}),e.setState({showLeftArrow:m}),e.setState(new v(0))),a&&s.shouldScrollToRight&&(e.setState(new L(1)),e.setState(new v(0)))},componentDidUpdate:function(e){var t=this.props||w;e.shadowArrowResize===t.shadowArrowResize&&e.windowWidth===t.windowWidth||this.initElWidth()},handleScroll:function(e){const t=this,r=t.state||w,o=t._table||w,l=t.props||w,s=l.mobileShadowConfig,i=r&&r.scrollableWidth,n=r&&r.wrapperWidth,c=r&&r.persistLeftScroll,d=a.qsa(S,g,o),h=i-n;let f="mouseenter"===(e&&e.type)?c:d?d.scrollLeft:0;l.windowWidth<u&&s&&(f=d?d.scrollLeft:0),t&&t.setState&&(t.setState({persistLeftScroll:f}),f>0?(t.setState({showLeftArrow:g}),t.setState(new L(1))):(t.setState({showLeftArrow:m}),t.setState(new R(g)),t.setState(new L(0))),f<h?(t.setState({showRightArrow:g}),t.setState(new v(1))):(t.setState({showRightArrow:m}),t.setState(new v(0))),f===h&&s&&(t.setState({showRightArrow:m}),t.setState(new v(0)),t.setState(new L(1))))},getScrollToWidth:function(e){const t=this,r=t&&t.state,o=r&&r.persistLeftScroll,l=r&&r.wrapperWidth,s=e?o-l:o+l,i=t&&t._table,n=a.qsa(x([S," ",".Table__sub-header th"]),m,i);let c,d=0,h=0,f=0;for(;(c=n[f++])&&(d=c.offsetWidth,!(h+d>s));)h+=d;return h},onScroll:n((function(e){var t=c(this,"props.onScroll");b(t)&&t(e,this._scroller)}),50),setRef:function(e){var t=this;t&&t.setState&&(e?t._table=e:delete t._table)},setScrollerRef:function(e){var t=this;t&&t.setState&&(e?t._scroller=e:delete t._scroller)},render:function(){const e=this,t=e&&e.state,r=e&&e.props,o=r&&r.arrowScroll,a=r&&r.onScroll,i=t&&t.navHeight,n=r&&r.arrowOffsets||{},c=n&&n.top,d=r&&r.rowHeight,h=e&&e.arrowScrollLeft,u=e&&e.arrowScrollRight,p=e&&e.debounceScroll,w=e&&e.onMouseLeave,g=e&&e.onLeftArrowMouseEnter,m=e&&e.onLeftArrowMouseLeave,S=e&&e.onRightArrowMouseEnter,x=e&&e.onRightArrowMouseLeave,y=e&&e.handleScroll,R=t&&t.leftOpacity,_=t&&t.rightOpacity,L=t&&t.showLeftArrow,v=t&&t.showRightArrow,N=t&&t.leftArrowAlt,C=t&&t.rightArrowAlt,O=t&&t.isFixedScrollArrow,j=e.setRef,H=e.setScrollerRef,E=new A(R),M=new A(_),W=r&&r.children,D=W&&W.props,k=D&&D.headings||[],F=D&&D.subheaders||[],B=D&&D.caption;let q,P,U,z,$,I,V,K=d&&T[d]||4;return k.length&&(K+=24),F.length&&(K+=24),B!==s.NULL&&(K+=28),I=30-K+i,V=s.mix({},n),V.top=c?30-K+c:I,$={top:0},$.top=K,O?z={overflow:"hidden"}:(q={onScroll:p,onMouseEnter:y,onMouseLeave:w},P={onMouseEnter:g,onMouseLeave:m},U={onMouseEnter:S,onMouseLeave:x}),l.jsxs("div",{className:"Table__ScrollerWrapper relative overflow-hidden",ref:j,...q,children:[o&&L&&l.jsx("div",{className:"Table__ArrowContainer",style:V,children:l.jsx("div",{className:"Table__Arrow Table__Arrow--left",style:$,onClick:h,children:l.jsx(f,{type:"filter",icon:"caret__left",alt:N,...P})})}),o&&v&&l.jsx("div",{className:"Table__ArrowContainer",style:V,children:l.jsx("div",{className:"Table__Arrow Table__Arrow--right",style:$,onClick:u,children:l.jsx(f,{type:"filter",icon:"caret__right",alt:C,...U})})}),l.jsx("div",{className:"Table__Shadow--left",style:E}),l.jsx("div",{className:r.showScroll?"Table__Scroller":null,ref:H,style:z,onScroll:b(a)?e.onScroll:null,children:W}),l.jsx("div",{className:"Table__Shadow--right",style:M})]})},arrowScroll:function(e,t){const r=this,o=r&&r.state,l=r&&r._table,s=a.qsa(S,g,l),i=o&&o.wrapperWidth,n=o&&o.scrollableWidth,c=o&&o.isFixedScrollArrow,d=n-i,h=r.getScrollToWidth(e);h<d&&e?r.animateArrows(c,h,s,{showLeftArrow:m,showRightArrow:g,rightOpacity:1,leftOpacity:0}):r.animateArrows(c,n,s,{showLeftArrow:g,showRightArrow:m,rightOpacity:0,leftOpacity:1})},arrowScrollLeft:function(e){return this.arrowScroll(g,e)},arrowScrollRight:function(e){return this.arrowScroll(m,e)},animateScroll:function(e,t){const r=this&&this.state,o=r&&r.persistLeftScroll,l=e-o>0?25:-25;let s,a=Math.abs(e-o);!function r(o){o<=a?(s=Math.pow(Math.log(o+2),2),setTimeout((function(){o+=25,t.scrollLeft+=l,r(o)}),s)):t.scrollLeft=e}(0)},animateArrows:function(e,t,r,o){let l=this,s=o||w,a=s.showLeftArrow,i=s.showRightArrow,n=s.rightOpacity,c=s.leftOpacity;e?l.setState({persistLeftScroll:t,showLeftArrow:a,showRightArrow:i,rightOpacity:n,leftOpacity:c}):l.setState({persistLeftScroll:t}),l.animateScroll(t,r)},displayName:"Table__Shadow"})),e.exports=o},114857:function(e,t,r){e.exports=r(908332)},58171:function(e,t,r){var o=r(667294),l=r(450239),s=o.isValidElement,a=o.cloneElement,i=o.createElement,n=l.isFunc;e.exports=function(e,t,r){var o,l,c=null;return null!=e?(l=(o=e.type)&&o.displayName||"",c=!t||l&&l===t.displayName?s(e)?a(e,r):e:i(t,r,e)):!e&&t&&n(t)&&(c=i(t,r||{},null)),c}},84753:function(e,t,r){var o=r(456029),l=r(753325),s=r(267206);e.exports=function(e,t){return e&&e.length?o(e,s(t,2),l):void 0}},909146:function(e,t,r){"use strict";r.r(t)},70288:function(e,t,r){"use strict";r.r(t)}}]);