export let WasmObj;
let Exports;
let TD = new TextDecoder();
let TE = new TextEncoder();
export let regex_to_dfa = function (str) {
  const err_msg = document.getElementById("error-message");
  err_msg.innerHTML = "";
  err_msg.style.display = "none";
  if (str.length == 0) {
    err_msg.innerHTML = "Regex string should not be empty.";
    err_msg.style.display = "initial";
    return;
  }
  Exports.WasmFreeAll();
  const enc_str = TE.encode(str);
  const alloc_mem = Exports.WasmAlloc(enc_str.byteLength);
  const mem_view = new Uint8Array(Exports.memory.buffer, alloc_mem, enc_str.byteLength);
  mem_view.set(enc_str);
  document.getElementById("transitions").classList.add("no-fsm-yet");
  document.getElementById("fsm-type").classList.add("no-fsm-yet");
  Exports.RegexToDFA(alloc_mem, enc_str.byteLength);
  Exports.WasmFree(alloc_mem);
  const ss_view = new Uint32Array(Exports.memory.buffer, Exports.StatesStrings.value, 3);
  const ss_len_view = new Uint32Array(Exports.memory.buffer, Exports.StatesStringsLen.value, 3)
  return [
    new Uint32Array(Exports.memory.buffer, ss_view[0], ss_len_view[0]),
    new Uint32Array(Exports.memory.buffer, ss_view[1], ss_len_view[1]),
    new Uint32Array(Exports.memory.buffer, ss_view[2], ss_len_view[2]),
  ];
}
async function JSPrint(BufferAddr, Len, Type) {
  const string = TD.decode(new Uint8Array(Exports.memory.buffer, BufferAddr, Len));
  if (Type == 0) {
    console.log(string);
  } else if (Type == 1) {
    console.warn(string);
  } else {
    const err_msg = document.getElementById("error-message");
    err_msg.innerHTML += string.replace(/\n/g, "<br>"); //Because HTML doesn't output \n
    err_msg.style.display = "initial";
    console.error(string);
  }
}
await WebAssembly.instantiateStreaming(fetch("./regex_dfa_converter.wasm"), {
  env: {
    JSPrint,
  },
},).then(result => {
  WasmObj = result;
  Exports = result.instance.exports;
});