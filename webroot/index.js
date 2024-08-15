export let WasmObj;
let Exports;
let TD = new TextDecoder();
let TE = new TextEncoder();
export let regex_to_dfa = function (str) {
  if(str.length==0){
    console.error("Regex string must be greater than 0");
    return;
  }
  Exports.WasmFreeAll();
  const enc_str = TE.encode(str);
  const alloc_mem = Exports.WasmAlloc(enc_str.byteLength);
  const mem_view = new Uint8Array(Exports.memory.buffer, alloc_mem, enc_str.byteLength);
  mem_view.set(enc_str);
  try {
    Exports.RegexToDFA(alloc_mem, enc_str.byteLength);
  } finally {
    Exports.WasmFree(alloc_mem);
  }
}
async function JSPrint(BufferAddr, Len, Type) {
  const string = TD.decode(new Uint8Array(Exports.memory.buffer, BufferAddr, Len));
  if (Type == 0) console.log(string);
  else if (Type == 1) console.warn(string);
  else console.error(string);
}
await WebAssembly.instantiateStreaming(fetch("./regex_dfa_converter.wasm"), {
  env: {
    JSPrint,
  },
},).then(result => {
  WasmObj = result;
  Exports = result.instance.exports;
});