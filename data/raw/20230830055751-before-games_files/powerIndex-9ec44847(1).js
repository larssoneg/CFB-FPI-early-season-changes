var _____WB$wombat$assign$function_____ = function(name) {return (self._wb_wombat && self._wb_wombat.local_init && self._wb_wombat.local_init(name)) || self[name]; };
if (!self.__WB_pmw) { self.__WB_pmw = function(obj) { this.__WB_source = obj; return this; } }
{
  let window = _____WB$wombat$assign$function_____("window");
  let self = _____WB$wombat$assign$function_____("self");
  let document = _____WB$wombat$assign$function_____("document");
  let location = _____WB$wombat$assign$function_____("location");
  let top = _____WB$wombat$assign$function_____("top");
  let parent = _____WB$wombat$assign$function_____("parent");
  let frames = _____WB$wombat$assign$function_____("frames");
  let opener = _____WB$wombat$assign$function_____("opener");

(self.webpackChunkfitt=self.webpackChunkfitt||[]).push([[1322],{97393:function(e,n,t){"use strict";var s=t(917434),o=t.n(s),i=t(252249);t.n(i)().load("powerIndex",o())},291027:function(e,n,t){var s=t(785893);t(667294);const o=t(45697),i=t(294184),a=t(775143),r=t(515166),c=t(450239).emptyObj;function l(e,n,t){let o=e;return n&&(o=s.jsx(a,{to:n,"data-clubhouse-uid":t,children:e})),o}function p(e){let n=e||c,t=n.team||c,o=n.isMobile,a=t.links,p=t.logo,d=t.displayName,f=t.abbrev,w=t.uid,u=o?f:d,m=p?function(e,n){return s.jsx(r,{src:e,alt:n,name:n,height:20,width:20,size:"sm"})}(p,d):null,b=m?l(m,a):null,g=l(u,a,w),h=n.align,x=i("TeamCell flex",{"align-center":"center"===h,"align-start":"start"===h||!h});return s.jsxs("div",{className:x,children:[s.jsx("span",{className:"pr3 TeamLink__Logo",children:b}),s.jsx("span",{className:"TeamLink__Name",children:g})]})}t(637920),p.propTypes={team:o.object.isRequired,isMobile:o.bool},p.defaultProps={team:{}},e.exports=p},240780:function(e,n,t){var s=t(785893);const o=t(667294),i=t(903896),a=t(45697),r=t(702753),c=t(450239),l=t(45831).Tabs,p=t(601541).Options,d=c.cstr,f=c.emptyAry,w=c.emptyObj,u=c.isFunc,m=[":league",":pageType","_","view/:view","season/:season","group/:group","sort/:sort","dir/:dir"];let b;b=i({displayName:"Filters",defaultProps:{lists:{},defaults:{}},propTypes:{lists:a.object},redirectConference:function(e){let n,t=this.props||w,s=t.handleRedirect,o=t.defaults||w,i=d(o.conference);n=new p("group",m,{group:i}),u(s)&&s(n,e)},redirectSeason:function(e){let n,t=this.props||w,s=t.handleRedirect,o=t.defaults||w,i=d(o.season);n=new p("season",m,{season:i}),u(s)&&s(n,e)},render:function(){let e=this,n=e.props||w,t=n.lists||w,i=t.conferences||f,a=t.views||f,c=t.seasons||f,p=n.selected||w,d=p.viewIdx,u=p.conference,m=p.season,b="mt3 mr2";return s.jsxs(o.Fragment,{children:[s.jsx(l,{data:a,activeTab:d,dynamic:!0,className:"mt5 mb3"}),s.jsxs("div",{className:"flex flex-wrap",children:[c.length>1?s.jsx(r,{list:c,defaultValue:m,onChange:e.redirectSeason,className:b}):null,i.length?s.jsx(r,{list:i,defaultValue:u,onChange:e.redirectConference,className:b}):null]})]})}}),e.exports=b},964231:function(e,n,t){var s=t(785893),o=t(667294),i=t(450239),a=t(834248).PropTypes,r=t(955818),c=o.Fragment,l=t(45697),p=t(294184),d=t(822154),f=i.emptyAry,w=i.emptyObj,u=t(1112),m=t(796783),b=t(279132),g=d.THEMES.DARK,h=t(923992).format,x=i.passThru,E=i.NULL;function j(e,n){var t=e.notes||w,o=t.noteByView||"",i=t.noteWithLink||w,a=i.label||"",l=i.href||"",d=e.dictionary||f,j=d.length,y=(n&&n.translate||x)("Last Updated:"),S=e.updatedDateRaw,N=`${y} ${h(S,"MMMM D, YYYY")}`,T=(n&&n.theme)===g,F=p({"brdr-clr-gray-08":!T,"brdr-clr-heavy-metal":T}),A=p({"clr-gray-04":!T,"clr-boulder":T});return s.jsxs(c,{children:[s.jsxs("div",{className:`${F} n8 mt5 ${A} pt5 bt`,children:[N?s.jsx("div",{className:"footer__lastUpdated",children:N}):E,s.jsx("div",{className:"footer__selectionNotes mb3",children:o}),l?s.jsx(r,{to:l,className:"h8",children:a}):E]}),j?s.jsx(b,{is_9_3:!0,className:"mt4",children:s.jsxs(m,{col1:!0,children:[s.jsx("div",{className:`${F} bb`,children:s.jsx("span",{className:`n8 db pr3 ${A}`})}),s.jsx(u,{data:d,withWindowResize:!0,fullWidth:!0})]})}):null]})}j.contextTypes={theme:l.string,translate:l.func},j.propTypes={glossary:a.arrayOrObservableArray,hasLines:l.bool,updatedDateRaw:l.string,note:l.string,noteByView:l.string,noteWithLink:l.object},e.exports=j},571089:function(e,n,t){var s,o=t(734155),i=t(785893),a=(t(667294),t(450239)),r=t(45697),c=t(903896),l=t(149109),p=new(t(101972)),d=t(390231),f=d.getSportIDBySportAbbrev,w=d.isCollegeByLabel,u=t(834248),m=u.toJS,b=u.action,g=t(114857).FixedTable,h=t(388782),x=t(32613).productAPI,E=t(980054),j=t(804855),y=t(217376),S=t(957043),N=t(630998),T=t(435161),F=t(227361),A=t(763105),R=t(471640),L=a.first,O=a.cstr,P=a.mix,k=a.cnum,v=a.passThru,I=a.emptyAry,D=a.emptyObj,_=a.shallowEq,C=a.TRUE,W=a.FALSE,M="Show More",B="fpi.fpi",K="context.translate";(s=c({displayName:"PowerIndexTable",getInitialState:function(){let e=F(this,"props.rows"),n=F(this,K,v);return P({},{page:1,loadingText:n(M),isLoading:W,rows:e},{noRecurse:C})},componentDidUpdate:function(e){let n=this||D,t=e||D,s=n.props||D,o=_(t.params,s.params),i=_(t.rows,s.rows);o&&i||n.setState({rows:s.rows,page:1})},handleLoadMoreOnClick:function(){let e=this,n=e.state||D,t=e.props||D,s=t.appConfig,o=n.page,i=k(o)+1,a=t.params,r=(t.selected||D).sort||D,c=r.stat||B,l=r.direction,p=S(c,":",l),d=P({},[a,{page:i,sort:p}]);e.setState({isLoading:C}),x.getData("powerIndex",d,s).then(e.handleNextStats).catch(e.handleError)},render:function(){let e=this,n=e.state||D,t=n.loadingText,s=n.isLoading,o=n.page,a=n.rows||I,r=e.props||D,c=r.isMobile,l=r.table||D,p=r.selected||D,d=p.league,f=w(d),u=l.headers||D,b=m(u.colgroups,W),x=T(b,((e,n)=>0===n?e-1:e)),S=c&&f?x:b,F=u.subheaders,O=A(u.subheaders,(e=>e&&"CONF"!==e.label)),P=r.totalPages||1,k=P>1&&o<P,v=c&&f?O:F,_=y.buildHeaders(u.headers,S),C=y.buildSubheaders(v,p),M=(p.sort||D).stat||B,K=R(M,".")[1]||"fpi",U=N(v,["name",K])+1,V=L(S),Y=`mt4 Table2__title--remove-capitalization PowerIndex__Table--${r.view||"fpi"} league-${d}`,H=i.jsx("div",{className:"mt5",children:i.jsx(h,{content:"No Data Available"})}),G=i.jsx("div",{className:"relative mv5 pa3",children:i.jsx(j,{size:"sm"})});return i.jsxs("div",{children:[a.length?i.jsx(g,{className:Y,colgroups:S,headings:_,subheaders:C,rows:a,fixedLeft:V,sortedCol:U,shouldScroll:!0}):H,k&&!s&&i.jsx(E,{loadNextPage:e.handleLoadMoreOnClick,text:t}),s&&G]})},handleNextStats:b((function(e){let n=this,t=n.props||D,s=n.context||D,o=s.translate||v,i=s.fanData,a=n.state||D,r=a.page,c=a.rows,l=k(r)+1,d=(e.data||D).teams,w=(t.table||D).statics,u=t.isMobile,m=t.categoriesData,b={stats:t.configStats},g=t.selected||D,h=O(f(g.league)),x=p.parseTeamsStatsList(d,b,m,w),E=y.buildRows(x,u,i,h,w)||I;E.length?n.setState({page:l,rows:S(c,E),isLoading:W,loadingText:o(M)}):n.handleError()})),handleError:function(){let e=F(this,K,v);this.setState({isLoading:W,loadingText:e("Try Again")})}})).contextTypes={translate:r.func,fanData:r.object},e.exports=o.env.TEST_CONTEXT?s:l(s,"app")},758928:function(e,n,t){var s=t(785893),o=(t(667294),t(450239)),i=t(294184),a=t(45697),r=t(514625);function c(e){let n=e.trend,t=e.pos||"",a=e.align||"trend tc",c=o.cnum(n),l=o.isNum(c),p=Math.abs(c),d=l&&c>0||!1,f=l?p:n,w=d?"arrow__up":"arrow__down",u=t?`${w}__${t}`:void 0,m={positive:l&&d,negative:l&&!d},b=`${i({plus:l&&d,minus:l&&!d})} ${f}`,g=i(a,m);return s.jsxs("div",{className:g,id:u,"aria-label":b,children:[l?s.jsx(r,{icon:w,"aria-labelledby":u}):null,f]})}t(187474),c.propTypes={trend:a.string,customStyles:a.object},e.exports=c},640593:function(e,n,t){var s=t(785893),o=(t(667294),t(45697)),i=t(514625);function a(e){return s.jsx(i,{className:"ArrowIcon__TableSort",icon:`caret__${e.type}`})}a.propTypes={type:o.oneOf(["up","down"])},e.exports=a},25974:function(e,n,t){var s=t(785893);t(667294);const o=t(45697),i=t(294184),a=t(40266).paramValues,r=a.DIR_ASC,c=a.DIR_DESC,l=t(450239).emptyObj,p=t(775143),d=t(640593);function f(e){let n=e||l,t=n.curSorting,o=n.curDirection,a=n.type,f=n.text,w=n.isLink,u=n.desc,m=n.styles,b=n.link,g=i(n.className,{underline:w}),h=function(e,n,t){let o;return n===e&&(t===r&&(o=s.jsx(d,{type:"up"})),t===c&&(o=s.jsx(d,{type:"down"}))),o}(t,a,o);return s.jsx("span",{className:g,title:u,style:m,children:w&&b?s.jsxs(p,{classes:"clr-gray-01",to:b,children:[f,h]}):s.jsx("span",{children:f})})}f.propTypes={curSorting:o.string,curDirection:o.string,islink:o.bool,type:o.string,text:o.string.isRequired},f.displayName="HeaderLink",e.exports=f},90933:function(e,n,t){var s=t(785893);t(667294);const o=t(45697),i=t(294184);function a(e){let n=e.className,t=e.value,o=e.hasColor,a=i(n,{"clr-negative":o&&t<0,"clr-positive":o&&t>=0});return s.jsx("div",{className:a,children:t})}a.propTypes={className:o.string,value:o.string.isRequired},e.exports=a},217376:function(e,n,t){var s=t(785893),o=(t(667294),t(294184)),i=t(450239),a=t(125907),r=a.Table__Heading,c=a.Table__Row,l=t(435161),p=t(639693),d=t(957043),f=t(471640),w=t(25974),u=t(90933),m=t(291027),b=t(758928),g=t(40266).paramValues,h=i.emptyAry,x=i.emptyObj,E=i.strReplace,j=i.FALSE,y={SORT_DESC:g.DIR_DESC,NO_GROUP:"none",POWER_INDEX:"fpi",DOUBLE_DASH:"--",DASH:"-",DEFAULT_TEAM_ID:"-1",EMPTY_STR:"",PERIOD:".",SPACE:" ",COL_SPAN:1,ZERO_STR:"0"};e.exports={buildRows:function(e,n,t,i,a){let r=a||x;return l(e,((e,a)=>{let f,w,g=e||x,h=g.conference||y.EMPTY_STR,E=function(e,n){return l(e,(e=>{let t=e||x,o=t.name||y.EMPTY_STR,i=t.isTrend,a=t.value,r=i&&a===y.ZERO_STR?y.DOUBLE_DASH:a;return i?s.jsx(b,{trend:r,align:"trend",pos:n},o):s.jsx(u,{value:r},o)}))}(g.stats,a),S=g.team||x,N=S.id||y.DEFAULT_TEAM_ID,T=S.uid||y.EMPTY_STR,F=j,A=T?`team-${T}`:y.EMPTY_STR,R=T?`conf-${T}`:y.EMPTY_STR,L=r.team&&s.jsx(m,{team:S,isMobile:n},A),O=r.conference&&!n&&s.jsx(u,{value:h},R),P=p([L,O]);try{F=t&&t.isFollowingTeam(N,i)}catch(e){}return w=o({"is-favorite":F}),f=d(P,E),s.jsx(c,{className:w,children:f})}))},buildHeaders:function(e,n){let t=n||h;return l(e,((e,n)=>{let o=e||y.EMPTY_STR,i=t[n]||y.COL_SPAN;return s.jsx(r,{className:"tc",colSpan:i,children:o},n)}))},buildSubheaders:function(e,n){let t=(n||x).sort||x,o=t.direction||y.SORT_DESC,i=t.stat||y.EMPTY_STR,a=f(i,y.PERIOD)[1]||y.POWER_INDEX;return l(e,(e=>{let n=e||x,t=n.group,i=`group-${t?E(t,y.SPACE,y.DASH):y.NO_GROUP}`;return s.jsx(w,{className:i,curSorting:a,curDirection:o,type:n.name,text:n.label,isLink:n.isLink,desc:n.description,link:n.link})}))}}},606915:function(e,n,t){var s=t(785893);const o=t(667294),i=t(45697),a=t(956877),r=t(450239),c=t(252628),l=t(240780),p=t(571089),d=t(964231),f=t(217376),w=t(390231).getSportIDBySportAbbrev,u=o.Fragment,m=r.cstr,b=r.emptyObj;function g(e,n){let t=e||b,o=(n||b).fanData,i=t.isMobile,r=t.powerIndex||b,g=t.params||b,h=r.filters,x=r.defaults,E=r.selected||b,j=m(w(E.league)),y=r.metadata||b,S=y.completeConfigStats,N=y.categoriesData,T=y.totalPages,F=r.pageHeading||"Football Power Index",A=t.handleRedirect,R=r.table||b,L=R.statics,O=R.stats,P=t.guestProfile,k=t.isWebview,v=r.notes,I=c(r.dictionary),D=g.view||"",_=f.buildRows(O,i,o,j,L),C=r.lastUpdated;return s.jsxs(u,{children:[s.jsx(a,{title:F,className:"mb5",isWebview:k}),s.jsx(l,{lists:h,defaults:x,selected:E,handleRedirect:A}),s.jsx(p,{table:R,selected:E,isMobile:i,guestProfile:P,view:D,rows:_,params:g,fanData:o,totalPages:T,configStats:S,categoriesData:N}),s.jsx(d,{params:g,isWebview:k,dictionary:I,notes:v,updatedDateRaw:C})]})}t(853980),g.propTypes={content:i.object},g.contextTypes={fanData:i.object,translate:i.func},e.exports=g},804855:function(e,n,t){e.exports=t(297163)},980054:function(e,n,t){var s=t(785893);t(667294);let o,i=t(45697),a=t(450239),r=t(294184),c=a.passThru,l=a.emptyObj,p=a.noop,d=t(684967).evtCancel,f=t(775143);o=t(903896)({displayName:"LoadMore",render:function(){let e=this||l,n=e.props||l,t=e.context||l,o=n.text||"Show More",i=t.translate||c,a=n.className,p=r("tc mv5 loadMore",a);return s.jsx("div",{className:p,children:s.jsx(f,{className:"loadMore__link",to:"#",onClick:e.handleClick,children:i(o)})})},handleClick:function(e){let n=(this.props||l).loadNextPage||p;d(e),n()}}),o.propTypes={loadNextPage:i.func,text:i.string},o.contextTypes={translate:i.func},e.exports=o},515166:function(e,n,t){var s=t(785893),o=(t(667294),t(801402)),i=t(450239),a=t(292448),r=i.emptyObj;e.exports=function(e){var n,t=e||r,i=t.height,c=t.width,l=t.srcHeight||i,p=t.srcWidth||c,d=t.name||null,f=t.alt||null,w=t.imgProps||r,u=t.src||null,m=t.size||"lg",b=t.figure||w.figure||!1;return n=o(u,{h:l,w:p}),u=o(u,{h:2*l,w:2*p}),s.jsx(a,{src:u,defaultImg:n,alt:f,title:d,size:m,useLazy:t.useLazy,figure:b,...w})}},125907:function(e,n,t){e.exports=t(114857)},471424:function(e,n,t){var s=t(857395),o=t(101972),i=t(835065),a=t(372418),r=t(950889),c=t(32613),l=t(390231),p=t(601541).handler,d=l.LEAGUE_NAMES,f=t(450239),w=t(481003),u=t(518882),m=c.productAPI,b=f.emptyObj,g=f.mix,h=f.noop,x=l.isCollegeByLabel,E="powerIndex",j={nfl:"2015","college-football":"2005","mens-college-basketball":"2008",nba:"2022",wnba:"2023"};let y;y=new s({componentName:"PowerIndex",config:u,fetchData:function(e,n){let t=n||b,s=t.league,o=((u.getConfigByLeague||h)(s)||b).defaults||b,i=o.view,a=t.view||i,r=(o.sort||b)[a],c=t.sort?{}:r,l=g({},[t,c]),p=g({},[t,{filterBy:E,startingseason:j[s]}]),d=[];return d.push(m.getData(E,l,e,{allowError:!0})),d.push(m.getData("seasonsDropdownV3",p,e)),x(s)&&d.push(m.getData("conferences",t,e)),Promise.all(d)},parser:o,metaData:r,updateState:function(e){return e},render:w,instanceAug:{handleRedirect:p}}),y.register({type:d.COLLEGE_FOOTBALL,parser:i}),y.register({type:d.MENS_COLLEGE_BASKETBALL,parser:a}),e.exports=y},370267:function(e,n,t){const s=t(11055),o=s.Stat,i=s.Group,a="fpi",r="efficiencies",c={label:"What is ESPN's College Football Power Index?",href:"https://web.archive.org/web/20230829202716/https://www.espn.com/blog/statsinfo/post/_/id/122612/an-inside-look-at-college-fpi"},l=[{param:"fpi",text:"FPI",note:"fpi.projections.note.extend",noteWithLink:c},{param:"resume",text:"Resume",note:"fpi.projections.note.extend",noteWithLink:c},{param:"efficiencies",text:"Efficiencies",note:"fpi.efficiencies.note.extend"}],p=new i(a,[new o("numwins","W-L",0,null,"numlosses","W-L",null,"numties")]," "),d={fpi:new i(null,[p,new i(a,[new o("fpi","FPI",1),new o("fpirank","RK",0,{sortAscending:!0}),new o("rankchange7days","TREND",null,{hideSort:!0,isTrend:!0})],"POWER INDEX"),new i(a,[new o("projectedw","PROJ W-L",1,null,"projectedl"),new o("probwinout","WIN OUT%",1,{base100:!0}),new o("prob6wins","6WINS%",1,{base100:!0}),new o("probwindiv","WIN DIV%",1,{base100:!0}),new o("probwinconf","WIN CONF%",1,{base100:!0}),new o("probmakeplayoffs","PLAYOFF%",1,{base100:!0}),new o("probmaketitlegame","MAKE NC%",1,{base100:!0}),new o("probwintitle","WIN NC%",1,{base100:!0})],"PROJECTIONS")]),resume:new i(null,[new i("resume",[new o("accomplishmentrank","SOR",0,{sortAscending:!0}),new o("fpirank","FPI",0,{sortAscending:!0}),new o("APRank/CFPRank","AP/CFP",0,{hideSort:!0,sortAscending:!0}),new o("avgsosrank","SOS",0,{sortAscending:!0}),new o("sosremainingrank","REM SOS",0,{sortAscending:!0}),new o("gamecontrolrank","GC",0,{sortAscending:!0}),new o("avgingamewprank","AVGWP",0,{sortAscending:!0})],"RANKS")]),efficiencies:new i(null,[new i(r,[new o("totefficiency","EFF",1),new o("totefficiencyrank","RNK",0,{sortAscending:!0})],"OVERALL",!0),new i(r,[new o("offefficiency","EFF",1),new o("offefficiencyrank","RNK",0,{sortAscending:!0})],"OFFENSE",!0),new i(r,[new o("defefficiency","EFF",1),new o("defefficiencyrank","RNK",0,{sortAscending:!0})],"DEFENSE",!0),new i(r,[new o("stefficiency","EFF",1),new o("stefficiencyrank","RNK",0,{sortAscending:!0})],"SPECIAL TEAMS",!0)])};e.exports={views:l,defaults:{conference:"80",view:"fpi",sort:{fpi:{sort:"fpi.fpi",dir:"desc"},resume:{sort:"resume.accomplishmentrank",dir:"asc"},efficiencies:{sort:"efficiencies.totefficiency",dir:"desc"}}},powerIndexHeaders:d,staticColumns:{team:{label:"Team"},conference:{label:"CONF"}}}},518882:function(e,n,t){let s=t(450239);e.exports={getConfigByLeague:function(e,n){let o,i;try{o=t(409784)(`./${e}.js`)}catch(e){}return i=s.isFunc(o)?o(n):o,s.mix({},i)}}},434692:function(e,n,t){let s=t(11055),o=t(450239),i=s.Stat,a=s.Group,r="bpi",c=o.NULL,l=o.TRUE,p={bpi:new a(c,[new a(r,[new i("wins","W-L",0,c,"losses")]," "),new a(r,[new i("bpi","BPI",1),new i("bpirank","BPI RK",0,{sortAscending:l}),new i("bpisevendaychangerank","TREND",c,{hideSort:l,isTrend:l}),new i("bpioffense","OFF",1),new i("bpidefense","DEF",1)],"POWER INDEX"),new a(r,[new i("projtotalwins","OVR W-L",c,{skipFormat:l},"projtotallosses"),new i("projconfwins","CONF W-L",c,{skipFormat:l},"projconflosses"),new i("chancewinconfortie","WIN CONF%",c,{skipFormat:l}),new i("sosremrank","REM SOS RK",c,{sortAscending:l,skipFormat:l})],"PROJECTIONS")]),resume:new a(c,[new a(r,[new i("wins","W-L",0,c,"losses")]),new a("resume",[new i("sorrank","SOR RK",0,{sortAscending:l}),new i("projectedtournamentseed","SOR SEED",0,{sortAscending:l}),new i("projectedtournamentorder","SOR S-CURVE",0,{sortAscending:l}),new i("top50bpiwins","QUAL WINS",c,{skipFormat:l},"top50bpilosses"),new i("sospastrank","SOS RK",0,{sortAscending:l}),new i("sosoutofconfpastrank","NC SOS",0,{sortAscending:l})])]),tournament:new a(c,[new a("tournament",[new i("projectedtournamentseedactual","SEED",1),new i("tournamentregion","REGION",c,{hideSort:l,skipFormat:l}),new i("chancencaachampion","TITLE WIN",c,{skipFormat:l}),new i("chancechampgame","CHAMP GM",c,{skipFormat:l}),new i("chancefinal4","FINAL FOUR",c,{skipFormat:l}),new i("chanceelite8","ELITE 8",c,{skipFormat:l}),new i("chancesweet16","SWEET 16",c,{skipFormat:l}),new i("chanceroundof32","RD32",c,{skipFormat:l})])])};e.exports={views:[{param:"bpi",text:"BPI",note:"bpi.projections.note.extend"},{param:"resume",text:"Resume",note:"bpi.projections.note.extend"},{param:"tournament",text:"Tournament",note:"bpi.projections.note.extend"}],defaults:{conference:"50",view:"bpi",sort:{bpi:{sort:"bpi.bpi",dir:"desc"},resume:{sort:"resume.sorrank",dir:"asc"},tournament:{sort:"tournament.chancencaachampion",dir:"desc"}}},powerIndexHeaders:p,staticColumns:{team:{label:"Team"},conference:{label:"CONF"}}}},766706:function(e,n,t){let s=t(11055),o=t(450239),i=s.Stat,a=s.Group,r="bpi",c="playoffs",l=o.NULL,p=o.TRUE,d={bpi:new a(l,[new a(r,[new i("numwins","W-L",0,l,"numlosses")]," "),new a(r,[new i("bpi","BPI",1),new i("bpirank","BPI RK",0,{sortAscending:p}),new i("bpioffense","OFF",1),new i("bpidefense","DEF",1),new i("playoffbpi","PBPI",1)],"POWER INDEX"),new a(r,[new i("offtalent","OFF",1),new i("deftalent","DEF",1)],"TALENT")]),projections:new a(l,[new a(r,[new i("numwins","W-L",0,l,"numlosses")]),new a("projections",[new i("projectedw","OVR W-L",1,l,"projectedl"),new i("probwindiv","WIN DIV%",1),new i("probmakeplayoffs","PLAYOFF%",1),new i("top6seed","TOP6%",1),new i("playinchance","PLAYIN%",1),new i("playoffseed","PROJ SEED",1,{sortAscending:p}),new i("projdraftslot","PROJ DRAFT",1,{sortAscending:p}),new i("probno1draftpick","1ST PICK%",1)])]),playoffs:new a(l,[new a(c,[new i("playoffbpi","PBPI",1),new i("playoffoff","POFF",1),new i("playoffdef","PDEF",1)],"POWER INDEX"),new a(c,[new i("probmakeplayoffs","PLAYOFF%",1),new i("probmakeconfsemi","MAKE CS%",1),new i("probmakeconfchamp","MAKE CF%",1),new i("probmaketitlegame","FINALS%",1),new i("probwintitle","WIN TITLE%",1)],"PLAYOFFS")])};e.exports={views:[{param:"bpi",text:"BPI",note:"bpi.projections.note.extend.nba"},{param:"projections",text:"Projections",note:"bpi.projections.note.extend.nba"},{param:"playoffs",text:"Playoffs",note:"bpi.projections.note.extend.nba"}],defaults:{view:"bpi",sort:{bpi:{sort:"bpi.bpi",dir:"desc"},projections:{sort:"projections.projectedw",dir:"desc"},playoffs:{sort:"playoffs.playoffbpi",dir:"desc"}}},powerIndexHeaders:d,staticColumns:{team:{label:"Team"}}}},417916:function(e,n,t){const s=t(11055),o=s.Stat,i=s.Group,a="fpi",r="efficiencies",c={label:"What is ESPN's NFL Football Power Index?",href:"https://web.archive.org/web/20230829202716/https://www.espn.com/blog/statsinfo/post/_/id/123048/a-guide-to-nfl-fpi"},l=new i(a,[new o("numwins","W-L-T",0,null,"numlosses","W-L-T",null,"numties")]," "),p={fpi:new i(null,[l,new i(a,[new o("fpi","FPI",1),new o("fpirank","RK",0,{sortAscending:!0}),new o("rankchange7days","TREND",null,{hideSort:!0,isTrend:!0}),new o("epaoffense","OFF",1),new o("epadefense","DEF",1),new o("epaspecialteams","ST",1)],"POWER INDEX"),new i(a,[new o("avgsosrank","SOS",0,{sortAscending:!0}),new o("sosremainingrank","REM SOS",0,{sortAscending:!0}),new o("avgingamewprank","AVGWP",0,{sortAscending:!0})],"RANKS")]),projections:new i(null,[new i(a,[new o("numwins","W-L-T",0,null,"numlosses","W-L-T",null,"numties")]),new i("projections",[new o("projectedw","PROJ W-L",1,null,"projectedl"),new o("probmakeplayoffs","PLAYOFF%",1,{base100:!0}),new o("probwindiv","WIN DIV%",1,{base100:!0}),new o("probmakedivplayoffs","MAKE DIV%",1,{base100:!0}),new o("probmakeconfchamp","MAKE CONF%",1,{base100:!0}),new o("probmaketitlegame","MAKE SB%",1,{base100:!0}),new o("probwintitle","WIN SB%",1,{base100:!0})])]),efficiencies:new i(null,[l,new i(r,[new o("totefficiency","EFF",1),new o("totefficiencyrank","RNK",0,{sortAscending:!0})],"OVERALL",!0),new i(r,[new o("offefficiency","EFF",1),new o("offefficiencyrank","RNK",0,{sortAscending:!0})],"OFFENSE",!0),new i(r,[new o("defefficiency","EFF",1),new o("defefficiencyrank","RNK",0,{sortAscending:!0})],"DEFENSE",!0),new i(r,[new o("stefficiency","EFF",1),new o("stefficiencyrank","RNK",0,{sortAscending:!0})],"SPECIAL TEAMS",!0)])},d=[{param:"fpi",text:"FPI",note:"fpi.projections.note",noteWithLink:c},{param:"projections",text:"Projections",note:"fpi.projections.note",noteWithLink:c},{param:"efficiencies",text:"Efficiencies",note:"fpi.efficiencies.note"}];e.exports={views:d,defaults:{view:"fpi",sort:{fpi:{sort:"fpi.fpi",dir:"desc"},projections:{sort:"projections.projectedw",dir:"desc"},efficiencies:{sort:"efficiencies.totefficiency",dir:"desc"}}},powerIndexHeaders:p,staticColumns:{team:{label:"Team"}}}},271186:function(e,n,t){let s=t(11055),o=t(450239),i=s.Stat,a=s.Group,r="bpi",c=o.NULL,l=o.TRUE,p={bpi:new a(c,[new a(r,[new i("numwins","W-L",0,c,"numlosses")]," "),new a(r,[new i("bpi","BPI",1),new i("bpirank","BPI RK",0,{sortAscending:l}),new i("bpioffense","OFF",1),new i("bpidefense","DEF",1)],"POWER INDEX")]),projections:new a(c,[new a(r,[new i("numwins","W-L",0,c,"numlosses")]),new a("projections",[new i("projectedw","OVR W-L",1,c,"projectedl"),new i("chancemakeplayoffs","PLAYOFF%",1),new i("projdraftslot","PROJ DRAFT",1,{sortAscending:l})])]),playoffs:new a(c,[new a("playoffs",[new i("chancemakeplayoffs","PLAYOFF%",1),new i("chancemakesemifinals","MAKE SF%",1),new i("chancemakefinals","FINALS%",1),new i("chancechampion","WIN TITLE%",1)],"PLAYOFFS")])};e.exports={views:[{param:"bpi",text:"BPI",note:"bpi.projections.note.extend.wnba"},{param:"projections",text:"Projections",note:"bpi.projections.note.extend.wnba"},{param:"playoffs",text:"Playoffs",note:"bpi.projections.note.extend.wnba"}],defaults:{view:"bpi",sort:{bpi:{sort:"bpi.bpi",dir:"desc"},projections:{sort:"projections.projectedw",dir:"desc"},playoffs:{sort:"playoffs.chancemakeplayoffs",dir:"desc"}}},powerIndexHeaders:p,staticColumns:{team:{label:"Team"}}}},917434:function(e,n,t){const s=t(471424),o=t(660225),i={components:{SponsoredLinks:t(186746)}};e.exports=o(s,i)},481003:function(e,n,t){var s=t(785893);const o=t(450239),i=o.emptyObj,a=t(227361),r=(t(667294),t(606915)),c=t(868214),l=t(375329),p=l.Card,d=l.Card__Content,f=t(506537),w=f.TABLET[0],u=f.DESKTOP_LABEL,m=t(822154).WEBVIEW;e.exports=function(){var e=this.props||i,n=e.state||i,t=n.viewport,l=(n.routing||i).params||i,f=l.modifier===m,b=t&&o.cnum(t.width,0,0),g=n.page||i,h=a(g,"meta.canonical"),x=a(g,"outbrain.id"),E=g.taboola,j=g.type,y=g.content||i,S=(e.components||i).SponsoredLinks,N=this.handleRedirect||o.noop,T=(n.app||i).device,F=b?b<=w:!o.strMatch(T,u),A=a(e,"guest.profile");return s.jsxs(c,{windowSize:b,children:[s.jsx(p,{children:s.jsx(d,{children:s.jsx(r,{powerIndex:y,params:l,isMobile:F,handleRedirect:N,windowWidth:b,isWebview:f,guestProfile:A})})}),S?s.jsx(S,{taboola:E,pageType:j,id:x,src:h,isHorizontal:!0}):null]})}},11055:function(e,n,t){const s=t(40266).paramValues,o=s.DIR_DESC,i=s.DIR_ASC;e.exports={Group:function(e,n,t,s){e&&(this.category=e),n&&(this.stats=n),t&&(this.title=t),s&&(this.showGroup=s)},Stat:function(e,n,t,s,a,r,c,l){let p=this,d=s||{},f=d.skipFormat,w=d.sortAscending,u=d.removeInteger,m=d.base100,b=d.isTrend,g=d.hideSort,h=d.isPercent;p.name=e,p.label=n,p.decimals=t||0,p.sortDirection=w?i:o,p.base100=m,p.hideSort=g,p.isTrend=b,f&&(p.skipFormat=!0),u&&(p.removeInteger=!0),a&&(p.secondName=a),l&&(p.thirdName=l),r&&(p.displayName=r),c&&(p.sortingCategory=c),h&&(p.isPercent=h)}}},637920:function(e,n,t){"use strict";t.r(n)},187474:function(e,n,t){"use strict";t.r(n)},853980:function(e,n,t){"use strict";t.r(n)},409784:function(e,n,t){var s={"./college-football.js":370267,"./index.js":518882,"./mens-college-basketball.js":434692,"./nba.js":766706,"./nfl.js":417916,"./wnba.js":271186};function o(e){var n=i(e);return t(n)}function i(e){if(!t.o(s,e)){var n=new Error("Cannot find module '"+e+"'");throw n.code="MODULE_NOT_FOUND",n}return s[e]}o.keys=function(){return Object.keys(s)},o.resolve=i,e.exports=o,o.id=409784}},function(e){e.O(0,[3242,9264,4857,2708,7613,6746,1992,6756,8639,1794],(function(){return 97393,e(e.s=97393)})),e.O()}]);

}
/*
     FILE ARCHIVED ON 20:27:16 Aug 29, 2023 AND RETRIEVED FROM THE
     INTERNET ARCHIVE ON 00:22:54 Sep 08, 2023.
     JAVASCRIPT APPENDED BY WAYBACK MACHINE, COPYRIGHT INTERNET ARCHIVE.

     ALL OTHER CONTENT MAY ALSO BE PROTECTED BY COPYRIGHT (17 U.S.C.
     SECTION 108(a)(3)).
*/
/*
playback timings (ms):
  captures_list: 148.187
  exclusion.robots: 0.068
  exclusion.robots.policy: 0.059
  cdx.remote: 0.062
  esindex: 0.009
  LoadShardBlock: 121.06 (3)
  PetaboxLoader3.datanode: 85.097 (4)
  load_resource: 92.734
  PetaboxLoader3.resolve: 67.986
*/