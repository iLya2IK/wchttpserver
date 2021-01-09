const OK = "OK";
const BAD = "BAD";
const NEW = "NEW";
const SAME = "SAME";
const VAL_CONNECTION_STATE = 1;
const VAL_LIFETIME_VALUE = 2;
const VAL_LAUNCH_STATE = 3;
const CCS_CONNECTED = 1; 
const CLS_RUN = 1; 

var esc = {src : null, config : null, username : ""}


/* fetch replacement */
function fastpost(method, params, successfunc) {
    var xhr = new XMLHttpRequest();
    xhr.open("POST", method, true);
    xhr.setRequestHeader('Content-Type', 'application/json; charset=utf-8');
    let content = JSON.stringify(params);
    xhr.onreadystatechange = () => {
        if(xhr.readyState === XMLHttpRequest.DONE && xhr.status === 200) 
          successfunc(xhr.responseText);      
    };
    (params)? xhr.send(content) : xhr.send();
  }

/******************************************************************************\
 * SERVER FUNCTIONS
/******************************************************************************/

function wcGetClientInt(apos, successfunc) {     
    let request = { pos : apos }
    fastpost("./wcGetClientInt.json", request, successfunc);
}

function wcDisconnect(successfunc) {
    fastpost("./wcDisconnect.json", null, successfunc);     
}

function wcLaunch(successfunc) {
    fastpost("./wcLaunch.json", null, successfunc);     
}

function wcStop(successfunc) {
    fastpost("./wcStop.json", null, successfunc);     
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
    if (!isAuthorized()) 
      return button_cancel.click();
    
    wcDisconnect(text => {
      let status = JSON.parse(text).status;
      (status == OK) ? updateConnectionState() : dropState();                
    });     
    /*console.log('try to disconnect')*/
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
  
  let request = {
      VAL_USERNAME : inputs["login"].value,
      VAL_USERPSW : inputs["password"].value
  }
      
  fastpost("./wcConnectToServer.json", request, text => {
        let status = JSON.parse(text).status;
        (status == OK)? startSynTimer() : dropState();                
        form_auth.classList.remove(S_ACT);
    });  
}

// close auth form on escape
document.onkeyup = event => {
  if( form_auth.classList.contains(S_ACT) && event.key == "Escape") {
    button_cancel.click()
  }
}

let syntimer = null;

function doSync(text) {              
  let synevent = JSON.parse(text);                 
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
  startSynTimer();
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

function startSynTimer() { 
  if  (syntimer == null) {
    needToReconnect = true;
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
      let text = e.data;
      doSync(text);
    }      
  } else {
      stopSynTimer();
      startSynTimer();
  }    
}

function stopSynTimer() { 
  if (syntimer != null) {
    clearTimeout(synreconnecttimeout);
    needToReconnect=false;
    syntimer.close();  
    syntimer = null;
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
