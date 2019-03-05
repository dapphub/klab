// TODO - move this to client
var last_rule = null;
const parse = {
  REACHINIT: data => ({data: data[0]}),
  EXECINIT: data => ({data: data[0]}),
  REACHTARGET: data => ({data: data[0]}),
  REACHPROVED: data => {
    return {
      nodeid: data[0]
    }
  },
  IMPLICATION: data => ({
    lhs: data[0].split("_")[0],
    rhs: data[0].split("_")[1]
  }),
  // Z3RESULT: data => ({
  //   query:  data[0],
  //   rule:   data[1],
  //   implication: data[2],
  //   nodeid: data[3],
  //   result: data[4],
  // }),
  // step: data => {
  //   let d = data[1].split("_");
  //   return {
  //     from: d[0]+"_"+d[1],
  //     to: d[2]+"_"+d[3]
  //   }
  //   console.log("step");
  // },
  SRULE: data => {
    let d = data[0].split("_");
    return {
      from: d[1]+"_"+d[2],
      to: d[3]+"_"+d[4],
      rule: d[0]
    }
  },
  RULE: data => {
    let d = data[0].split("_");
    return {
      from: d[1]+"_"+d[2],
      to: d[3]+"_"+d[4],
      rule: d[0]
    }
  },
  CLOSE: data => {
    return ({})
  }
}


module.exports = msg => {
  let data = msg.data.split(" ")
  let [ time , type ] = data
  let rest = data.slice(2)
  return type in parse
    && Object.assign({type: type}, {time: time}, parse[type](rest))
    || {type: "unknown", data}
}

