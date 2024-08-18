document.addEventListener("DOMContentLoaded", function () {
  button = document.getElementById("compile-button");
  regex = document.getElementById("regex");
  teststr = document.getElementById("teststr");
  transitions = document.getElementById("transitions");
  transitions_body = document.getElementById("transitions-body");
  regex_patterns = document.getElementById("regex-patterns");
  fsm_type = document.getElementById("fsm-type");
  button.onclick = parse_regex;
  for (let i = 0; i < 3; i++) {
    fsm_type.children[i].onclick = function (e) {
      if (e.target.classList.contains('selected-fsm')) return;
      fsm_type.children[states_i].classList.remove('selected-fsm');
      e.target.classList.add('selected-fsm');
      states_i = i;
      if (fsm_type.classList.contains('no-fsm-yet')) return;
      parse_states(states[i]);
    };
  }
  for(const [k,v] of Object.entries(regex_patterns_obj)){
    const option=document.createElement("option");
    option.value=v;
    option.innerHTML=k;
    regex_patterns.appendChild(option);
  }
  regex_patterns.onchange=compile_pattern;
  regex.onfocus=change_list_empty;
});
function compile_pattern(e){
  if(e.target.value!=="") regex.value=e.target.value;
}
function change_list_empty(e){
  regex_patterns.value='';
}
const regex_patterns_obj={
  "":"",
  "Time HH:MM:SS, 12-hour format, leading 0 hour optional":"(0?[1-9]|1[0-2]):([0-5]\\d):([0-5]\\d) ?([AP]M|[ap]m)",
  "Time HH:MM:SS, 24-hour format":"([01]\\d|2[0-3]):([0-5]\\d):([0-5]\\d)",
  "Username where alphanumeric strings and - are allowed, 3 to 16 characters only, and letter is the 1st character":"[A-Za-z][\\w\\-]{2,15}",
  "IP Version 4 (IPv4)":"((25[0-5]|2[0-4]\\d|1\\d{2}|[1-9]\\d?|0)\\.){3}(25[0-5]|2[0-4]\\d|1\\d{2}|[1-9]\\d?|0)",
  "IP Version 6 (IPv6), leading zeroes required and :: zeroes compression disallowed":"([\\dA-Fa-f]{4}:){7}([\\dA-Fa-f]{4})",
  "IP Version 6 (IPv6), leading zeroes optional and :: zeroes compression allowed":"((([\\dA-Fa-f]{1,4}:){7}([\\dA-Fa-f]{1,4}|:))|(([\\dA-Fa-f]{1,4}:){1,7}:)|(([\\dA-Fa-f]{1,4}:){1,6}:[\\dA-Fa-f]{1,4})|(([\\dA-Fa-f]{1,4}:){1,5}(:[\\dA-Fa-f]{1,4}){1,2})|(([\\dA-Fa-f]{1,4}:){1,4}(:[\\dA-Fa-f]{1,4}){1,3})|(([\\dA-Fa-f]{1,4}:){1,3}(:[\\dA-Fa-f]{1,4}){1,4})|(([\\dA-Fa-f]{1,4}:){1,2}(:[\\dA-Fa-f]{1,4}){1,5})|(([\\dA-Fa-f]{1,4}:)(:[\\dA-Fa-f]{1,4}){1,6})|(:((:[\\dA-Fa-f]{1,4}){1,7}|:)))(%.+)?",
};
let button;
let regex;
let teststr;
let transitions;
let transitions_body;
let states;
let regex_patterns;
let fsm_type;
let states_i = 0;
function parse_regex() {
  states = window.wasm.regex_to_dfa(regex.value);
  if (states === undefined) return;
  fsm_type.classList.remove("no-fsm-yet");
  transitions.classList.remove("no-fsm-yet");
  parse_states(states[states_i]);
}
//Parse states to HTML.
function parse_states(state_arr) {
  const num_states = state_arr[0];
  let arr_i = 1;
  let state_i = 0;
  var accept_arr = [];
  transitions_body.innerHTML = '';
  while (state_i < num_states) {
    const state_num = state_arr[arr_i++];
    const accept = state_arr[arr_i++];
    const num_transitions = state_arr[arr_i++];
    const num_transitions_bytes = state_arr[arr_i++];
    console.assert(typeof (state_num) === "number" &&
      state_num == state_i &&
      typeof (accept) === "number" &&
      typeof (num_transitions) === "number" &&
      typeof (num_transitions_bytes) === "number",
      "Incorrectly reading state bytes in code."
    );
    accept_arr.push(accept == 1);
    arr_i += num_transitions_bytes;
    state_i++;
  }
  arr_i = 1;
  state_i = 0;
  while (state_i < num_states) {
    const state_num = state_arr[arr_i++];
    const accept = state_arr[arr_i++];
    const num_transitions = state_arr[arr_i++];
    state_arr[arr_i++];
    const tr = document.createElement("tr");
    transitions_body.appendChild(tr);
    tr.id = `s${state_num}`;
    const td_state = document.createElement("td");
    const td_transition = document.createElement("td");
    tr.appendChild(td_state);
    tr.appendChild(td_transition);
    const div_state = document.createElement("div");
    td_state.appendChild(div_state);
    if (accept_arr[state_num]) div_state.classList.add("accept-state");
    if (state_i == 0) {
      div_state.innerHTML = `${state_num} <em class="mark error">Error</em>`;
      const div_transition = document.createElement("div");
      td_transition.appendChild(div_transition);
      div_transition.href = "javascript:void(0);";
      div_transition.innerHTML = `[ <em class="mark">\\u0000</em> - <em class="mark">\\uffff</em> ] &#x2192; 0`;
      const state_f={to_state:0,f:move_to_state};
      div_transition.onclick = move_to_state.bind(state_f);
    } else {
      if (state_i == 1)
        div_state.innerHTML = `${state_num} <em class="mark">Init</em>`;
      else
        div_state.innerHTML = state_num;
      if (accept == 1) div_state.innerHTML += ` <em class="mark accept">Accept</em>`;
      let tr_i = 0;
      while (tr_i < num_transitions) {
        const to_state = state_arr[arr_i++];
        const type_tr = state_arr[arr_i++];
        const div_transition = document.createElement("div");
        td_transition.appendChild(div_transition);
        if (accept_arr[to_state]) div_transition.classList.add("accept-state");
        switch (type_tr) {
          case 0:
            div_transition.innerHTML = `<em class="mark epsilon">&#x025B;</em>`;
            break;
          case 1:
            const single_p = state_arr[arr_i++];
            div_transition.innerHTML = `${char_or_unicode(single_p)}`;
            break;
          case 2:
            const min = state_arr[arr_i++];
            const max = state_arr[arr_i++];
            div_transition.innerHTML = `[ ${char_or_unicode(min)} - ${char_or_unicode(max)} ]`;
            break;
          default:
            console.error("Incorrectly reading transition bytes in code.");
        }
        div_transition.innerHTML += ` &#x2192; ${to_state}`;
        const state_f={to_state:to_state,f:move_to_state};
        div_transition.onclick = move_to_state.bind(state_f);
        tr_i += 1;
      }
    }
    state_i++;
  }
}
function move_to_state(e){
  const html_to_state = document.getElementById(`s${this.to_state}`);
  html_to_state.scrollIntoView({ behavior: 'instant', block: 'center' });
  html_to_state.classList.add('highlight-state');
  setTimeout(() => html_to_state.classList.remove('highlight-state'), 500);
}
function char_or_unicode(num) {
  if (32 <= num && num <= 126) {
    return `<em class="mark">${String.fromCharCode(num)}</em>`
  } else {
    return `<em class="mark">\\u${num.toString(16).padStart(4, '0')}</em>`;
  }
}