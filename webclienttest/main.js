const OK = "OK";
const BAD = "BAD";
const NEW = "NEW";
const SAME = "SAME";
const VAL_CONNECTION_STATE = 1;
const VAL_LIFETIME_VALUE = 2;
const VAL_LAUNCH_STATE = 3;
const CCS_CONNECTED = 1; 
const CLS_RUN = 1;
const MODE_EVENT_SOURCE = 1;
const MODE_WEB_SOCKET = 2;

var CUR_MODE = 0;
var AUTHO_MODE = 0;
var esc = {src : null, config : null, username : ""}

function toUTF8Array(str) {
    var utf8 = [];
    for (var i=0; i < str.length; i++) {
        var charcode = str.charCodeAt(i);
        if (charcode < 0x80) utf8.push(charcode);
        else if (charcode < 0x800) {
            utf8.push(0xc0 | (charcode >> 6), 
                      0x80 | (charcode & 0x3f));
        }
        else if (charcode < 0xd800 || charcode >= 0xe000) {
            utf8.push(0xe0 | (charcode >> 12), 
                      0x80 | ((charcode>>6) & 0x3f), 
                      0x80 | (charcode & 0x3f));
        }
        else {
            i++;
            charcode = 0x10000 + (((charcode & 0x3ff)<<10)
                      | (str.charCodeAt(i) & 0x3ff))
            utf8.push(0xf0 | (charcode >>18), 
                      0x80 | ((charcode>>12) & 0x3f), 
                      0x80 | ((charcode>>6) & 0x3f), 
                      0x80 | (charcode & 0x3f));
        }
    }
    return utf8;
}

function fastpost(method, params, successfunc) {
    if (CUR_MODE === MODE_EVENT_SOURCE) {
        fastpost_http(method, params, successfunc);
    } else
    if (CUR_MODE === MODE_WEB_SOCKET) {
        fastpost_websocket(method, params, successfunc);
    }        
}

/* fetch replacement */
function fastpost_http(method, params, successfunc) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", method, true);
    let content = JSON.stringify(params);
    xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');    
    if (content.length > 200) {
        var plain = new Uint8Array(toUTF8Array(content));
        content = new Zlib.Deflate(plain, {compressionType: Zlib.Deflate.CompressionType.DYNAMIC}).compress();            
        xhr.setRequestHeader('Content-Encoding', 'deflate');        
    }
    xhr.onreadystatechange = () => {
        if(xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) 
          successfunc(xhr.responseText);      
    };
    (params)? xhr.send(content) : xhr.send();
}
  
function fastpost_websocket(method, params, successfunc) {
    create_jsonrpc_req(method, params, false, successfunc);
}  

var JSONRPC_RID = 0;
var jsonrpc_requests = new Map();
  
function create_jsonrpc_req(method, params, ispersist, successfunc) {
    let arid = JSONRPC_RID++;    
    request = {persist:ispersist, func:successfunc, body:{rid:arid, uri:method, data:params}};
    jsonrpc_requests.set(arid, request);
    (synwstimer)&&synwstimer.send(JSON.stringify(request.body));
}

/******************************************************************************\
 * SERVER FUNCTIONS
/******************************************************************************/

function wcGetClientInt(apos, successfunc) {     
    fastpost("./wcGetClientInt.json", { pos : apos }, successfunc);
}

function wcDisconnect(successfunc) {
    fastpost_http("./wcDisconnect.json", null, successfunc);     
}

function wcLaunch(successfunc) {
    fastpost_http("./wcLaunch.json", null, successfunc);     
}

function wcStop(successfunc) {
    fastpost_http("./wcStop.json", null, successfunc);     
}

// https://github.com/Modi34/Microbe template engine
let m=(...n)=>n.map(t=>this[t]=(...p)=>(n=document.createElement(t),p.map(p=>p+''=={}?Object.assign(n,p):p&&p.call?p(n):n.append(p)),n));

m('ul', 'li', 'h6', 'button', 'textarea', 'div');

/******************************************************************************\
 * SUPPORT-CHECK
/******************************************************************************/

let supNoSVG = false;

function checkSVGSupport() {
  const elements = document.getElementsByTagName("li")
  const buttons = document.getElementsByTagName("button")
  const backs = document.querySelectorAll(".backContainer")
  if (typeof SVGRect === "undefined") {
    supNoSVG = true;
    [...elements,...buttons,...backs].some(li => {li.classList.add('no_svg');});
  } else {
    [...elements,...buttons,...backs].some(li => {li.classList.remove('no_svg');});
  }
}

function checkSupport() { checkSVGSupport(); }

/******************************************************************************\
 * CONSTANTS
/******************************************************************************/
const S_RUNNING = 'running';
const S_AUTH = 'authorized';
const S_ACT = 'active';
const S_NOSVG = 'no_svg';

/******************************************************************************\
 * IDE TOOLS
/******************************************************************************/

function isAuthorized() {return document.body.classList.contains(S_AUTH);}
function isRunning() {return document.body.classList.contains(S_RUNNING);}

/******************************************************************************\
 * TOOLBAR
/******************************************************************************/

let click_toolbar = {
  button_connect(event) {
    if (!isAuthorized()) {
      AUTHO_MODE = MODE_EVENT_SOURCE;
      return button_cancel.click();
    }   
  },
  button_connect_ws(event) {
    if (!isAuthorized()) {
      AUTHO_MODE = MODE_WEB_SOCKET;
      return button_cancel.click();
    }   
  },  
  button_disconnect(event) {
    if (isAuthorized()) {
        wcDisconnect(text => {
        let status = JSON.parse(text).status;
        (status == OK) ? updateConnectionState() : dropState();                
        });     
    }
  },  
  button_run(event) {
    if (!isRunning()) {
        console.log('RUN');
        wcLaunch(text => {
            let status = JSON.parse(text).status;
            if (status == OK) updateLaunchState();});
    } else {
        console.log('STOP');
        wcStop(text => {
            let status = JSON.parse(text).status;
            if (status == OK) updateLaunchState();});
    }
  }
}

document.onclick = function (event) {
  click_toolbar[event.target.id]&&
    click_toolbar[event.target.id](event)
}

/******************************************************************************\
 * AUTH FORM
/******************************************************************************/

// since button_cancel of type reset it clears out inputs
// so I just click cancel button when I need to open or close auth form
button_cancel.onclick = function (event) {
  form_auth.classList.toggle(S_ACT)
}

form_auth.onsubmit = event => {
  event.preventDefault();
  var inputs = form_auth.elements
  var anAuthoMode = AUTHO_MODE;
  
  let request = {
      VAL_USERNAME : inputs["login"].value,
      VAL_USERPSW : inputs["password"].value,
      content : "some long string to test client-side compression. here is some important message for user. i think 200 chars is anough. here is additional utf8 symbols: бабушка, балалайка"
  }
      
  fastpost_http("./wcConnectToServer.json", request, text => {
        let status = JSON.parse(text).status;
        (status == OK)? reconnectSynTimer(anAuthoMode) : dropState();                
        form_auth.classList.remove(S_ACT);
    });  
}

function reconnectSynTimer(anAuthoMode) {
  if (anAuthoMode === MODE_WEB_SOCKET) {
    startWebSocketSynTimer();
  }
  if (anAuthoMode === MODE_EVENT_SOURCE) {
    startEventSourceSynTimer();
  }  
}

// close auth form on escape
document.onkeyup = event => {
  if( form_auth.classList.contains(S_ACT) && event.key == "Escape") {
    button_cancel.click()
  }
}

let syntimer = null;
let synwstimer = null;

function doSync(synevent) {                       
  for (let i in synevent.events) {
      // proceed event      
      let theevent = synevent.events[i];
      switch (theevent) {
          case VAL_CONNECTION_STATE : {
            updateConnectionState();
            break;
          }        
          case VAL_LIFETIME_VALUE : {
            updateLifeTime();
            break;            
          }
          case VAL_LAUNCH_STATE : {
            updateLaunchState();
            break;
          }
      }
  }
};

let synreconnecttimeout;

function isFunction(functionToCheck) {
  return functionToCheck && {}.toString.call(functionToCheck) === '[object Function]';
}

function debounce(func, wait) {    
    var waitFunc;

    return function() {
        if (isFunction(wait)) {
            waitFunc = wait;
        }
        else {
            waitFunc = function() { return wait };
        }

        var context = this, args = arguments;
        var later = function() {
            timeout = null;
            func.apply(context, args);
        };
        clearTimeout(synreconnecttimeout);
        synreconnecttimeout = setTimeout(later, waitFunc());
    };
}
let reconnectFrequencySeconds = 1;
let needToReconnect = false;
let reconnectFunc = debounce(function() {
  reconnectSynTimer();
  // Double every attempt to avoid overwhelming server
  reconnectFrequencySeconds *= 2;
  // Max out at ~1 minute as a compromise between user experience and server load
  if (reconnectFrequencySeconds >= 64) {
      reconnectFrequencySeconds = 64;
      dropState();      
  } else {
    // addToLog("Connection lost. Try to reconnect to server...")
  }
}, function() { return reconnectFrequencySeconds * 1000 });

function startEventSourceSynTimer() { 
  if  (syntimer == null) {
    needToReconnect = true;
    CUR_MODE = MODE_EVENT_SOURCE;
    syntimer = new EventSource('./wcDoSynchronizeEventSource.json');     
    syntimer.addEventListener("disconnect", function(event) {
      dropState();
    }); 
    syntimer.onopen = e=>{
      updateConnectionState();
      reconnectFrequencySeconds = 1;
    }
    syntimer.onerror = e=>{
      if (this.readyState === EventSource.CONNECTING) {
        // addToLog("Try to reconnect to server...")
      } else {          
        syntimer.close();
        if (needToReconnect) reconnectFunc();
      }
    }
    syntimer.onmessage = e=>{
      let synevent = JSON.parse(e.data);
      doSync(synevent);
    }      
  } else {
      stopSynTimer();
      startSynTimer();
  }    
}
function startWebSocketSynTimer() { 
  if  (syntimer == null) {
    needToReconnect = true;
    CUR_MODE = MODE_WEB_SOCKET;
    // Создаёт WebSocket - подключение.
    synwstimer = new WebSocket('wss://localhost:8080');
    synwstimer.addEventListener('open', function (event) { 
        create_jsonrpc_req("./wcDoSynchronizeWebSocket.json", {lstId: 0}, true, 
                           msg=>{      
                            if (msg.event == "disconnect") {
                                dropState();
                            } else {
                                let synevent = JSON.parse( msg ).data;
                                doSync(synevent);
                            }});
    });    
    synwstimer.addEventListener('error', function (event) {   
      if (synwstimer) {
        synwstimer.close();
        if (needToReconnect) reconnectFunc();      
      }
    });    
    synwstimer.addEventListener('message', function (event) {
      let response = JSON.parse(event.data);
      let request = jsonrpc_requests.get(response.rid);
      if (request) {
          request.func( response.data );
          if (!request.persist) {
              jsonrpc_requests.delete(response.rid);
          }
      } else {
          //close();?
      }
    });
  } else {
      stopSynTimer();
      startWebSocketSynTimer();
  }    
}

function stopSynTimer() {
  clearTimeout(synreconnecttimeout);
  needToReconnect=false;    
  jsonrpc_requests.clear();
  if (syntimer != null) {
    syntimer.close();  
    syntimer = null;
  }
  if (synwstimer != null) {
    synwstimer.close(1000);  
    synwstimer = null;    
  }
}
//
function dropState() {    
    stopSynTimer();    
    
    document.body.classList.remove(S_AUTH);      
}

function updateConnectionState() {
  wcGetClientInt(VAL_CONNECTION_STATE, res=>{
            let resv = JSON.parse(res);
            if (resv === CCS_CONNECTED) {
                document.body.classList.add(S_AUTH);                
                console.log('connected');
            } else {                            
                dropState();                
            }      
        }
    )
}

function updateLaunchState() {
  wcGetClientInt(VAL_LAUNCH_STATE, res=>{
            let resv = JSON.parse(res);
            if (resv === CLS_RUN) {
                document.body.classList.add(S_RUNNING);                
                console.log('connected');
            } else {                            
                document.body.classList.remove(S_RUNNING);                
                console.log('connected');          
            }      
        }
    )
}

function updateLifeTime() {
  wcGetClientInt(VAL_LIFETIME_VALUE, res=>{
            let resv = JSON.parse(res);
            let sec = Math.floor(resv / 1000);
            let msec = resv % 1000;
            time_area.innerHTML = `${sec}s ${msec}ms`;
        }
    )    
}

checkSupport();
stopSynTimer();
