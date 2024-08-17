document.addEventListener("DOMContentLoaded", function () {
  button = document.getElementById("form-button");
  regex = document.getElementById("regex");
  teststr = document.getElementById("teststr");
  transitions = document.getElementById("transitions-body");
  button.addEventListener("click", parse_regex);
});
let button;
let regex;
let teststr;
let transitions;
let states;
function parse_regex() {
  states = window.index.regex_to_dfa(regex.value);
  if (states === undefined) return;
  const states_html = parse_states(states.dfa_min);
  transitions.innerHTML = states_html.outerHTML;
}
//Parse states to HTML table.
function parse_states(state_arr) {
  const num_states = state_arr[0];
  let arr_i = 1;
  let state_i = 0;
  const tbody = document.createElement("tbody");
  var accept_arr = [];
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
    accept_arr.push(accept==1);
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
    tbody.appendChild(tr);
    const td_state = document.createElement("td");
    const td_transition = document.createElement("td");
    tr.appendChild(td_state);
    tr.appendChild(td_transition);
    const div_state = document.createElement("div");
    td_state.appendChild(div_state);
    if(accept_arr[state_num]) div_state.className = "accept-state";
    if (state_i == 0) {
      div_state.innerHTML = `${state_num} <em class="mark error">Error</em>`;
      const div_transition = document.createElement("div");
      td_transition.appendChild(div_transition);
      div_transition.href = "javascript:void(0);";
      div_transition.innerHTML = `[ <em class="mark">\\u0000</em> - <em class="mark">\\uffff</em> ] &#x2192; 0`;
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
        if(accept_arr[to_state]) div_transition.className = "accept-state";
        switch (type_tr) {
          case 0:
            div_transition.innerHTML = `<em class="mark epsilon">&#x025B;</em>`;
            break;
          case 1:
            const single_p = state_arr[arr_i++];
            div_transition.innerHTML = `[ ${char_or_unicode(single_p)} ]`;
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
        tr_i += 1;
      }
    }
    state_i++;
  }
  return tbody;
}
function char_or_unicode(num) {
  if (32 <= num && num <= 126) {
    return `<em class="mark">${String.fromCharCode(num)}</em>`
  } else {
    return `<em class="mark">\\u${num.toString(16).padStart(4,'0')}</em>`;
  }
}