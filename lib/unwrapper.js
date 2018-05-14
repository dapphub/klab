const {formatMemory} = require("./formatmemory.js");

let a = `4 :
bool2Word (
  #asWord (
    #range (
      _1079 [
        64 := #padToWidth (
          32 ,
          #asByteStack (
            #asWord (
              #take (
                32 ,
                #padToWidth (
                  32 ,
                  #asByteStack ( X )
                ) ++
                #padToWidth (
                  32 ,
                  #asByteStack ( N )
                ) ++
                .WordStack
              )
            )
          )
        )
      ] [
        96 :=
          #padToWidth (
            32 ,
            #asByteStack (
              #asWord (
                #take ( 32 ,
                  #drop ( 32 ,
                    #padToWidth ( 32 ,
                      #asByteStack ( X )
                    ) ++
                    #padToWidth ( 32 ,
                      #asByteStack ( N )
                    ) ++
                    .WordStack )
                )
              )
            )
          )
      ] [
        128 <- 0
      ] [
        129 <- 0
      ] [
        130 <- 0
      ] [
        131 <- 0
      ] [
        132 <- 0
      ] [
        133 <- 0
      ] [
        134 <- 0
      ] [
        135 <- 0
      ] [
        136 <- 0
      ] [
        137 <- 0
      ] [
        138 <- 0
      ] [
        139 <- 0
      ] [
        140 <- 0
      ] [
        141 <- 0
      ] [
        142 <- 0
      ] [
        143 <- 0
      ] [
        144 <- 0
      ] [
        145 <- 0
      ] [
        146 <- 0
      ] [
        147 <- 0
      ] [
        148 <- 3
      ] [
        149 <- 59
      ] [
        150 <- 46
      ] [
        151 <- 60
      ] [
        152 <- 159
      ] [
        153 <- 208
      ] [
        154 <- 128
      ] [
        155 <- 60
      ] [
        156 <- 232
      ] [
        157 <- 0
      ] [
        158 <- 0
      ] [
        159 <- 0
      ] ,
      127 ,
      32 ,
      .WordStack
      )
  )
  ==K 0
  andBool
    #asWord (
      #range ( _1079 [ 64 := #padToWidth ( 32 , #asByteStack ( #asWord ( #take ( 32 , #padToWidth ( 32 , #asByteStack ( X ) ) ++ #padToWidth ( 32 , #asByteStack ( N ) ) ++ .WordStack ) ) ) ) ] [
          96 := #padToWidth ( 32 ,
            #asByteStack ( #asWord (
              #take ( 32 , #drop ( 32 ,
                #padToWidth ( 32 , #asByteStack ( X ) ) ++
                #padToWidth ( 32 , #asByteStack ( N ) ) ++
                .WordStack ) ) ) ) )
          ] [ 128 <- 0 ] [ 129 <- 0 ] [ 130 <- 0 ] [ 131 <- 0 ] [ 132 <- 0 ] [ 133 <- 0 ] [ 134 <- 0 ] [ 135 <- 0 ] [ 136 <- 0 ] [ 137 <- 0 ] [ 138 <- 0 ] [ 139 <- 0 ] [ 140 <- 0 ] [ 141 <- 0 ] [ 142 <- 0 ] [ 143 <- 0 ] [ 144 <- 0 ] [ 145 <- 0 ] [ 146 <- 0 ] [ 147 <- 0 ] [ 148 <- 3 ] [ 149 <- 59 ] [ 150 <- 46 ] [ 151 <- 60 ] [ 152 <- 159 ] [ 153 <- 208 ] [ 154 <- 128 ] [ 155 <- 60 ] [ 156 <- 232 ] [ 157 <- 0 ] [ 158 <- 0 ] [ 159 <- 0 ]
        , 95 , 32 , .WordStack ) ) ==K 0
) :
WS`;

const c = "_1079 [ 64 := #padToWidth ( 32 , #asByteStack ( X ) ) ] [ 96 := #padToWidth ( 32 , #asByteStack ( N ) ) ] [ 128 -- 0 ] [ 129 -- 0 ] [ 130 -- 0 ] [ 131 -- 0 ] [ 132 -- 0 ] [ 133 -- 0 ] [ 134 -- 0 ] [ 135 -- 0 ] [ 136 -- 0 ] [ 137 -- 0 ] [ 138 -- 0 ] [ 139 -- 0 ] [ 140 -- 0 ] [ 141 -- 0 ] [ 142 -- 0 ] [ 143 -- 0 ] [ 144 -- 0 ] [ 145 -- 0 ] [ 146 -- 0 ] [ 147 -- 0 ] [ 148 -- 3 ] [ 149 -- 59 ] [ 150 -- 46 ] [ 151 -- 60 ] [ 152 -- 159 ] [ 153 -- 208 ] [ 154 -- 128 ] [ 155 -- 60 ] [ 156 -- 232 ] [ 157 -- 0 ] [ 158 -- 0 ] [ 159 -- 0 ]"

const empty = `_1079`;
const b = `_1079 [ 64 := #padToWidth ( 32 , #asByteStack ( X ) ) ] [ 96 := #padToWidth ( 32 , #asByteStack ( N ) ) ] [ 128 <- 0 ] [ 129 <- 0 ] [ 130 <- 0 ] [ 131 <- 0 ] [ 132 <- 0 ] [ 133 <- 0 ] [ 134 <- 0 ] [ 135 <- 0 ] [ 136 <- 0 ] [ 137 <- 0 ] [ 138 <- 0 ] [ 139 <- 0 ] [ 140 <- 0 ] [ 141 <- 0 ] [ 142 <- 0 ] [ 143 <- 0 ] [ 144 <- 0 ] [ 145 <- 0 ] [ 146 <- 0 ] [ 147 <- 0 ] [ 148 <- 3 ] [ 149 <- 59 ] [ 150 <- 46 ] [ 151 <- 60 ] [ 152 <- 159 ] [ 153 <- 208 ] [ 154 <- 128 ] [ 155 <- 60 ] [ 156 <- 232 ] [ 157 <- 0 ] [ 158 <- 0 ] [ 159 <- 0 ]`;

// parse memory in memory format
const parseMemory = b => (b
  .trim()
  .match(/\[ ([^\]]*) \]/g) || [])
  .map(s => s.replace(/\[ (.*) \]/, (a,e) => e))
  .map(s => {
    // case variable
    if( (/\:\=/).test(s) ) {
      let pos = parseInt(s.match(/^\d+ /)[0])
      let length = parseInt(s.replace(/^.*\#padToWidth \( (\d+).*$/, (a,e) => e))
      let name = s.replace(/^.*, (.*) \)$/, (a,e) => e);
      if((/^\#asByteStack/).test(name)) name = name.replace(/^#asByteStack \( (.*) \)$/, (a, e) => e)
      return [pos, length, name];
    // case byte
    } else if((/\<\-/).test(s)) {
      let pos = parseInt(s.match(/^\d+ /)[0])
      let byte = parseInt(s.replace(/^\d+ \<\- (\d+)$/, (a,e) => e)).toString(16)
      if(byte.length == 1) byte = "0" + byte;
      return [pos, 2, byte]
    } else if((/\-\-/).test(s)) {
      let pos = parseInt(s.match(/^\d+ /)[0])
      let byte = parseInt(s.replace(/^\d+ \-\- (\d+)$/, (a,e) => e)).toString(16)
      if(byte.length == 1) byte = "0" + byte;
      return [pos, 2, byte]
    } else {
      return s
    }
  })
  .sort((a, b) => a[0] - b[0])

// fill holes in memory
const normalize = w => {
  let maxMemorySlot = w.reduce( (a, e) => Math.max(a, e[0]+e[1]), 0)
  let i = 0;
  let currentLow = 0;
  let w_ = [];
  w.forEach(([s, l, n]) => {
    let d = s - currentLow;
    if(d > 0) {
      w_.push([currentLow, d, "0".repeat(d)]);
      currentLow += d;
    }
    currentLow += l;
    w_.push([s, l, n])
  })
  return w_;
}

// console.log(formatMemory(normalize(parseMemory(b))));

module.exports = {
  formatMemory: str => formatMemory(normalize(parseMemory( str )))
}







if(process.argv[1] === __filename) {
  console.log(c);
  console.log(parseMemory(c));
  console.log(normalize(parseMemory( c )));
  console.log(formatMemory(normalize(parseMemory( c ))));
}




