document.addEventListener("DOMContentLoaded", function () {
  compile_button = document.getElementById("compile-button");
  cancel_compilation = document.getElementById("cancel-compilation");
  cancel_compilation_body = document.getElementById("cancel-compilation-body");
  regex = document.getElementById("regex");
  test_regex_e = document.getElementById("test-regex");
  transitions = document.getElementById("transitions");
  transitions_body = document.getElementById("transitions-body");
  regex_patterns = document.getElementById("regex-patterns");
  fsm_type = document.getElementById("fsm-type");
  test_string_output = document.getElementById("test-string-output");
  compile_button.onclick = () => wasm_worker.postMessage(["regex_to_dfa", regex.value]);
  create_wasm_worker();
  cancel_compilation.onclick = () => {
    wasm_worker.terminate(); //Not sure if there's a possible "async" way of just cancelling inside the wasm file using the worker other than .terminate().
    create_wasm_worker();
    append_error("Cancelled compilation.");
    toggle_cancel(false);
  };
  test_regex_e.oninput = () => wasm_worker.postMessage(["test_string", test_regex_e.value]);
  test_regex_e.onfocus = dfa_min_and_test_regex;
  err_msg = document.getElementById("error-message");
  for (let i = 0; i < 3; i++) {
    const state_f = { i: i, f: select_fsm };
    fsm_type.children[i].onclick = select_fsm.bind(state_f);
  }
  for (const [k, v] of Object.entries(regex_patterns_obj)) {
    const option = document.createElement("option");
    option.value = v;
    option.innerHTML = k;
    regex_patterns.appendChild(option);
  }
  regex_patterns.onchange = compile_pattern;
  regex.onfocus = change_list_empty;
});
function create_wasm_worker() {
  wasm_worker = new Worker("wasm.js");
  wasm_worker.onmessage = e => worker_handler_module[e.data[0]](e.data[1]);
  wasm_worker.onerror = () => toggle_cancel(false);
}
const worker_handler_module = {
  toggle_cancel: toggle_cancel,
  set_compile_state: set_compile_state,
  parse_regex: parse_regex,
  append_error: append_error,
  test_regex: test_regex,
};
function select_fsm(e) {
  if (e.target.classList.contains('selected-fsm')) return;
  fsm_type.children[states_i].classList.remove('selected-fsm');
  e.target.classList.add('selected-fsm');
  states_i = this.i;
  if (fsm_type.classList.contains('no-fsm-yet')) return;
  parse_states(states[this.i]);
}
function compile_pattern(e) {
  if (e.target.value !== "") {
    regex.value = e.target.value;
    wasm_worker.postMessage(["regex_to_dfa", regex.value]);
  }
}
function change_list_empty(e) {
  regex_patterns.value = '';
}
const regex_patterns_obj = {
  "": "",
  "Time HH:MM:SS, 12-hour format, leading 0 hour optional": "(0?[1-9]|1[0-2]):([0-5]\\d):([0-5]\\d) ?([AP]M|[ap]m)",
  "Time HH:MM:SS, 24-hour format": "([01]\\d|2[0-3]):([0-5]\\d):([0-5]\\d)",
  "Username where alphanumeric strings and - are allowed, 3 to 16 characters only, and letter is the 1st character": "[A-Za-z][\\w\\-]{2,15}",
  "IP Version 4 (IPv4)": "((25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)\\.){3}(25[0-5]|(2[0-4]|1\\d|[1-9]|)\\d)",
  "IP Version 6 (IPv6), leading zeroes required and :: zeroes compression disallowed": "([\\dA-Fa-f]{4}:){7}([\\dA-Fa-f]{4})",
  "IP Version 6 (IPv6), leading zeroes optional and :: zeroes compression allowed": "((([\\dA-Fa-f]{1,4}:){7}([\\dA-Fa-f]{1,4}|:))|(([\\dA-Fa-f]{1,4}:){1,7}:)|(([\\dA-Fa-f]{1,4}:){1,6}:[\\dA-Fa-f]{1,4})|(([\\dA-Fa-f]{1,4}:){1,5}(:[\\dA-Fa-f]{1,4}){1,2})|(([\\dA-Fa-f]{1,4}:){1,4}(:[\\dA-Fa-f]{1,4}){1,3})|(([\\dA-Fa-f]{1,4}:){1,3}(:[\\dA-Fa-f]{1,4}){1,4})|(([\\dA-Fa-f]{1,4}:){1,2}(:[\\dA-Fa-f]{1,4}){1,5})|(([\\dA-Fa-f]{1,4}:)(:[\\dA-Fa-f]{1,4}){1,6})|(:((:[\\dA-Fa-f]{1,4}){1,7}|:)))(%.+)?",
};
let compile_button;
let cancel_compilation;
let cancel_compilation_body;
let regex;
let test_regex_e;
let transitions;
let transitions_body;
let states;
let regex_patterns;
let fsm_type;
let test_string_output;
let wasm_worker;
let err_msg;
let states_i = 0;
function parse_regex(data) {
  states = data;
  if (states === undefined) return;
  fsm_type.classList.remove("no-fsm-yet");
  transitions.classList.remove("no-fsm-yet");
  parse_states(states[states_i]);
}
function toggle_cancel(b) {
  compile_button.disabled = b;
  cancel_compilation.disabled = !b;
  cancel_compilation_body.style.display = (!b) ? "none" : "initial";
}
function set_compile_state() {
  err_msg.innerHTML = '';
  err_msg.style.display = "none";
  transitions.classList.add("no-fsm-yet");
  fsm_type.classList.add("no-fsm-yet");
  test_string_output.innerHTML = '';
}
function append_error(msg) {
  err_msg.textContent += msg;
  err_msg.style.display = "initial";
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
      div_transition.innerHTML = `[ <em class="mark">\\u0000</em> - <em class="mark">\\uffff</em> ] &#x2192; 0`;
      const state_f = { to_state: 0 };
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
        const state_f = { to_state: to_state };
        div_transition.onclick = move_to_state.bind(state_f);
        tr_i += 1;
      }
    }
    state_i++;
  }
}
function move_to_state() {
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
function dfa_min_and_test_regex() {
  const state_f = { i: 2 }; //DFA Min
  const e = { target: fsm_type.children[2] };
  select_fsm.bind(state_f)(e);
  wasm_worker.postMessage(["test_string", test_regex_e.value]);
}
function test_regex(tgs) {
  if (tgs.length == 0) return;
  test_string_output.innerHTML = "";
  const init_state = document.createElement("div");
  test_string_output.appendChild(init_state);
  init_state.innerHTML = `1 <em class="mark">Init</em>`;
  const init_state_f = { to_state: 1 };
  init_state.onclick = move_to_state.bind(init_state_f);
  const fs_i = tgs[0];
  const fs_accept = tgs[1];
  const tgs_byte_count = tgs[2];
  var tgs_i = 3;
  var key_i = 0;
  while (tgs_i - 3 < tgs_byte_count) {
    const tr_state = tgs[tgs_i++];
    const this_tr_div = document.createElement("div");
    const tr_state_f = { to_state: tr_state };
    this_tr_div.onclick = move_to_state.bind(tr_state_f);
    test_string_output.appendChild(this_tr_div);
    const is_accept = tgs[tgs_i++];
    if (tr_state == 0) {
      this_tr_div.classList.add("error");
      this_tr_div.innerHTML = `<em class="mark error">${test_regex_e.value[key_i++]}</em> &#x2192; 0`;
    } else {
      if (is_accept) this_tr_div.classList.add("accept");
      this_tr_div.innerHTML = `<em class="mark ${(is_accept) ? "accept" : ""}">${test_regex_e.value[key_i++]}</em> &#x2192; ${tr_state}`;
    }
  }
  const final_state = document.createElement("div");
  const AcceptOrError = (fs_accept == 1) ? "accept" : "error";
  final_state.classList.add(AcceptOrError);
  test_string_output.appendChild(final_state);
  final_state.innerHTML = `<em class="mark ${AcceptOrError}">${(fs_accept == 1) ? "Accepted" : "Rejected"}</em> ${fs_i}`;
  const final_state_f = { to_state: fs_i };
  final_state.onclick = move_to_state.bind(final_state_f);
  move_to_state.bind(final_state_f)(); //Also move to that state.
}