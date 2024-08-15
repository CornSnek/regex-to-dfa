export let WasmObj;
let Exports;
let TD=new TextDecoder();
let TE=new TextEncoder();
async function init(){
  await WebAssembly.instantiateStreaming(fetch("./regex_dfa_converter.wasm"),{
    env:{
      JSPrint,
    },
  },).then(result=>{
    WasmObj=result;
    Exports=result.instance.exports;
  });
}
async function JSPrint(BufferAddr,Len,Type){
    const string=TD.decode(new Uint8Array(Exports.memory.buffer,BufferAddr,Len));
    if(Type==0) console.log(string);
    else if(Type==1) console.warn(string);
    else console.error(string);
}
await init();