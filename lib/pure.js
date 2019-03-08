// TODO - move this to client
// stateful
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
  RULEATTEMPT: data => {
    let d = data[0].split('_')
    return {
      nodeid: d[1] + '_' + d[2],
      rule: d[0]
    };
  },
  CHECKINGCONSTRAINT: data => ({
    constraint: data[0],
  }),
  Z3QUERY: data => ({
    query: data[0]
  }),
  Z3RESULT: data => ({
    result: data[0]
  }),
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

