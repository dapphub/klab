pragma solidity ^0.4.23;
interface D0 {
  function slip(bytes32,address,int256) external;
  function move(address,address,int256) external;
  function suck(address,int256) external;
  function burn(int256) external;
  function grab(bytes32,address) external;
  function fold(bytes32,int256) external;
  function mold(bytes32,int256) external;
  function hold(bytes32,int256) external;
  function cage() external;
  function cope(int256) external;
  function rage() external returns (int256);
  function rope() external returns (int256,int256);
  function rave() external returns (int256);
  function geld(address) external returns (int256);
  function gold(bytes32,address) external returns (int256);
  function peek(bytes32,address) external returns (int256,int256);
  function look(bytes32) external returns (int256);
  function feel(bytes32) external returns (int256);
  function grip(bytes32) external returns (int256,int256);
  function frob(bytes32,int256,int256) external;
}
contract D0Impl {
  constructor () public {
    assembly {
      sstore(0, caller)
    }
  }
  function () public {
    assembly {
      let sig := div(calldataload(0), 0x100000000000000000000000000000000000000000000000000000000)
      if lt(sig, 0x69245009 /* cage() */) {
        if lt(sig, 0x2c80d13a /* look(bytes32) */) {
          if lt(sig, 0x19c3777b /* geld(address) */) {
            if lt(sig, 0x156b595a /* peek(bytes32,address) */) {
              if lt(sig, 0x0c0412ea /* burn(int256) */) {
                if eq(sig, 0x0af9d5e0 /* suck(address,int256) */) {
                  /*
                    reverts iff
                      || !sub_safe(V, delta_D)
                      || V - delta_D < 0
                      || !add_safe(D[u], delta_D)
                      || D[u] + delta_D < 0
                      (besides auth)
                  */
                  /* reverts if not root */
                  if iszero(eq(caller, sload(0))) { revert(0, 0) }
                  
                  /* let V_ = V - delta_D */
                  let V_ := isub(sload(11), calldataload(36))
                  
                  /* iff V_ >= 0 */
                  if slt(V_, 0) { revert(0, 0) }
                  
                  /* set V = V_ */
                  sstore(11, V_)
                  
                  /* let D_u = D[u] + delta_D */
                  let hash_0 := hash2(4, calldataload(4))
                  let D_u := iadd(sload(hash_0), calldataload(36))
                  
                  /* iff D_u >= 0 */
                  if slt(D_u, 0) { revert(0, 0) }
                  
                  /* set D[u] = D_u */
                  sstore(hash_0, D_u)
                  stop()
                }
                if eq(sig, 0x0bf330d6 /* hold(bytes32,int256) */) {
                  /* reverts iff Q_ < 0 (besides auth) */
                  /* reverts if not root */
                  if iszero(eq(caller, sload(0))) { revert(0, 0) }
                  
                  /* iff Q_ >= 0 */
                  if slt(calldataload(36), 0) { revert(0, 0) }
                  
                  /* set Q[i] = Q_ */
                  sstore(hash2(8, calldataload(4)), calldataload(36))
                  stop()
                }
              }
              if eq(sig, 0x0c0412ea /* burn(int256) */) {
                /*
                  reverts iff
                    || delta_D < 0
                    || !sub_safe(D[u], delta_D)
                    || D[u] - delta_D < 0
                    || !sub_safe(p, delta_D)
                    || p - delta_D < 0
                    (besides auth)
                */
                
                /* iff delta_D >= 0 */
                if slt(calldataload(4), 0) { revert(0, 0) }
                
                /* let D_u = D[u] - delta_D */
                let hash_0 := hash2(4, caller)
                let D_u := isub(sload(hash_0), calldataload(4))
                
                /* iff D_u >= 0 */
                if slt(D_u, 0) { revert(0, 0) }
                
                /* set D[u] = D_u */
                sstore(hash_0, D_u)
                
                /* let p_ = p - delta_D */
                let p_ := isub(sload(9), calldataload(4))
                
                /* iff p_ >= 0 */
                if slt(p_, 0) { revert(0, 0) }
                
                /* set p = p_ */
                sstore(9, p_)
                stop()
              }
            }
            if eq(sig, 0x156b595a /* peek(bytes32,address) */) {
              /* reverts iff 0 (besides auth) */
              
              /* get c[i][u], d[i][u] */
              mstore(64, sload(hash3(1, calldataload(4), calldataload(36))))
              mstore(96, sload(hash3(3, calldataload(4), calldataload(36))))
              return(64, 64)
            }
          }
          if eq(sig, 0x19c3777b /* geld(address) */) {
            /* reverts iff 0 (besides auth) */
            
            /* get D[u] */
            mstore(64, sload(hash2(4, calldataload(4))))
            return(64, 32)
          }
          if eq(sig, 0x27219087 /* move(address,address,int256) */) {
            /*
              reverts iff
                || !sub_safe(D[u], delta_D)
                || D[u] - delta_D < 0
                || !add_safe(D[v], delta_D)
                || D[v] + delta_D < 0
                (besides auth)
            */
            /* reverts if not root */
            if iszero(eq(caller, sload(0))) { revert(0, 0) }
            
            /* let D_u = D[u] - delta_D */
            let hash_0 := hash2(4, calldataload(4))
            let D_u := isub(sload(hash_0), calldataload(68))
            
            /* iff D_u >= 0 */
            if slt(D_u, 0) { revert(0, 0) }
            
            /* set D[u] = D_u */
            sstore(hash_0, D_u)
            
            /* let D_v = D[v] + delta_D */
            hash_0 := hash2(4, calldataload(36))
            let D_v := iadd(sload(hash_0), calldataload(68))
            
            /* iff D_v >= 0 */
            if slt(D_v, 0) { revert(0, 0) }
            
            /* set D[v] = D_v */
            sstore(hash_0, D_v)
            stop()
          }
        }
        if lt(sig, 0x4b62e11d /* gold(bytes32,address) */) {
          if lt(sig, 0x4914dd0b /* rope() */) {
            if eq(sig, 0x2c80d13a /* look(bytes32) */) {
              /* reverts iff 0 (besides auth) */
              
              /* get X[i] */
              mstore(64, sload(hash2(5, calldataload(4))))
              return(64, 32)
            }
            if eq(sig, 0x392404e9 /* mold(bytes32,int256) */) {
              /* reverts iff Y_ < 0 (besides auth) */
              /* reverts if not root */
              if iszero(eq(caller, sload(0))) { revert(0, 0) }
              
              /* iff Y_ >= 0 */
              if slt(calldataload(36), 0) { revert(0, 0) }
              
              /* set Y[i] = Y_ */
              sstore(hash2(6, calldataload(4)), calldataload(36))
              stop()
            }
          }
          if eq(sig, 0x4914dd0b /* rope() */) {
            /* reverts iff 0 (besides auth) */
            
            /* get P, p */
            mstore(64, sload(10))
            mstore(96, sload(9))
            return(64, 64)
          }
        }
        if eq(sig, 0x4b62e11d /* gold(bytes32,address) */) {
          /* reverts iff 0 (besides auth) */
          
          /* get C[i][u] */
          mstore(64, sload(hash3(2, calldataload(4), calldataload(36))))
          return(64, 32)
        }
        if eq(sig, 0x5a984ded /* frob(bytes32,int256,int256) */) {
          /*
            reverts iff
              || G
              || X[i] == 0
              || !add_safe(c[i][u], delta_c)
              || c[i][u] + delta_c < 0
              || !sub_safe(C[i][u], delta_c)
              || C[i][u] - delta_c < 0
              || !add_safe(d[i][u], delta_d)
              || d[i][u] + delta_d < 0
              || !add_safe(q[i], delta_d)
              || q[i] + delta_d < 0
              || !mul_safe(X[i], delta_d)
              || !add_safe(D[u], X[i] * delta_d)
              || D[u] + X[i] * delta_d < 0
              || !add_safe(p, X[i] * delta_d)
              || p + X[i] * delta_d < 0
              ||
                && (|| delta_d > 0 || delta_c < 0)
                &&
                  (|| !mul_safe(X[i], d[i][u] + delta_d)
                   || !mul_safe(Y[i], c[i][u] + delta_c))
              ||
                && (|| delta_d > 0 || delta_c < 0)
                && X[i] * (d[i][u] + delta_d) > Y[i] * (c[i][u] + delta_c)
              || && delta_d > 0 && !mul_safe(X[i], q[i] + delta_d)
              ||
                && delta_d > 0
                && (|| X[i] * (q[i] + delta_d) > Q[i] || p + X[i] * delta_d > P)
              (besides auth)
          */
          
          /* let X_ = X[i] */
          let X_ := sload(hash2(5, calldataload(4)))
          
          /* iff !G && X_ != 0 */
          if sload(12) { revert(0, 0) }
          if eq(X_, 0) { revert(0, 0) }
          
          /* let c_i_u = c[i][u] + delta_c */
          let hash_0 := hash3(1, calldataload(4), caller)
          let c_i_u := iadd(sload(hash_0), calldataload(36))
          
          /* iff c_i_u >= 0 */
          if slt(c_i_u, 0) { revert(0, 0) }
          
          /* set c[i][u] = c_i_u */
          sstore(hash_0, c_i_u)
          
          /* let C_i_u = C[i][u] - delta_c */
          hash_0 := hash3(2, calldataload(4), caller)
          let C_i_u := isub(sload(hash_0), calldataload(36))
          
          /* iff C_i_u >= 0 */
          if slt(C_i_u, 0) { revert(0, 0) }
          
          /* set C[i][u] = C_i_u */
          sstore(hash_0, C_i_u)
          
          /* let d_i_u = d[i][u] + delta_d */
          hash_0 := hash3(3, calldataload(4), caller)
          let d_i_u := iadd(sload(hash_0), calldataload(68))
          
          /* iff d_i_u >= 0 */
          if slt(d_i_u, 0) { revert(0, 0) }
          
          /* set d[i][u] = d_i_u */
          sstore(hash_0, d_i_u)
          
          /* let q_i = q[i] + delta_d */
          hash_0 := hash2(7, calldataload(4))
          let q_i := iadd(sload(hash_0), calldataload(68))
          
          /* iff q_i >= 0 */
          if slt(q_i, 0) { revert(0, 0) }
          
          /* set q[i] = q_i */
          sstore(hash_0, q_i)
          
          /* let X_idelta_d = X_ * delta_d */
          let X_idelta_d := imul(X_, calldataload(68))
          
          /* let D_u = D[u] + X_idelta_d */
          hash_0 := hash2(4, caller)
          let D_u := iadd(sload(hash_0), X_idelta_d)
          
          /* iff D_u >= 0 */
          if slt(D_u, 0) { revert(0, 0) }
          
          /* set D[u] = D_u */
          sstore(hash_0, D_u)
          
          /* let p_ = p + X_idelta_d */
          let p_ := iadd(sload(9), X_idelta_d)
          
          /* iff p_ >= 0 */
          if slt(p_, 0) { revert(0, 0) }
          
          /* set p = p_ */
          sstore(9, p_)
          
          /* iff
               delta_d <= 0 && delta_c >= 0 || X_ * d_i_u <= Y[i] * c_i_u */
          if or(sgt(calldataload(68), 0), slt(calldataload(36), 0)) {
            if
              sgt(
                imul(X_, d_i_u), imul(sload(hash2(6, calldataload(4))), c_i_u)
              ) {
              revert(0, 0)
            }
          }
          
          /* iff delta_d <= 0 || X_ * q_i <= Q[i] && p_ <= P */
          if sgt(calldataload(68), 0) {
            if sgt(imul(X_, q_i), sload(hash2(8, calldataload(4)))) {
              revert(0, 0)
            }
            if sgt(p_, sload(10)) { revert(0, 0) }
          }
          stop()
        }
      }
      if lt(sig, 0xa66d2bdf /* rave() */) {
        if lt(sig, 0x83f30253 /* feel(bytes32) */) {
          if lt(sig, 0x7dbf3135 /* cope(int256) */) {
            if eq(sig, 0x69245009 /* cage() */) {
              /* reverts iff 0 (besides auth) */
              /* reverts if not root */
              if iszero(eq(caller, sload(0))) { revert(0, 0) }
              
              /* set G = 1 */
              sstore(12, 1)
              stop()
            }
            if eq(sig, 0x7cdd3fde /* slip(bytes32,address,int256) */) {
              /*
                reverts iff || !add_safe(C[i][u], delta_C) || C[i][u] + delta_C < 0
                  (besides auth)
              */
              /* reverts if not root */
              if iszero(eq(caller, sload(0))) { revert(0, 0) }
              
              /* let C_i_u = C[i][u] + delta_C */
              let hash_0 := hash3(2, calldataload(4), calldataload(36))
              let C_i_u := iadd(sload(hash_0), calldataload(68))
              
              /* iff C_i_u >= 0 */
              if slt(C_i_u, 0) { revert(0, 0) }
              
              /* set C[i][u] = C_i_u */
              sstore(hash_0, C_i_u)
              stop()
            }
          }
          if eq(sig, 0x7dbf3135 /* cope(int256) */) {
            /* reverts iff O < 0 (besides auth) */
            /* reverts if not root */
            if iszero(eq(caller, sload(0))) { revert(0, 0) }
            
            /* iff O >= 0 */
            if slt(calldataload(4), 0) { revert(0, 0) }
            
            /* set P = O */
            sstore(10, calldataload(4))
            stop()
          }
        }
        if eq(sig, 0x83f30253 /* feel(bytes32) */) {
          /* reverts iff 0 (besides auth) */
          
          /* get Y[i] */
          mstore(64, sload(hash2(6, calldataload(4))))
          return(64, 32)
        }
        if eq(sig, 0x923e2431 /* rage() */) {
          /* reverts iff 0 (besides auth) */
          
          /* get G */
          mstore(64, sload(12))
          return(64, 32)
        }
      }
      if lt(sig, 0xf3436159 /* fold(bytes32,int256) */) {
        if lt(sig, 0xd46ea28f /* grab(bytes32,address) */) {
          if eq(sig, 0xa66d2bdf /* rave() */) {
            /* reverts iff 0 (besides auth) */
            
            /* get V */
            mstore(64, sload(11))
            return(64, 32)
          }
          if eq(sig, 0xaf96709b /* grip(bytes32) */) {
            /* reverts iff 0 (besides auth) */
            
            /* get Q[i], q[i] */
            mstore(64, sload(hash2(8, calldataload(4))))
            mstore(96, sload(hash2(7, calldataload(4))))
            return(64, 64)
          }
        }
        if eq(sig, 0xd46ea28f /* grab(bytes32,address) */) {
          /*
            reverts iff
              || !sub_safe(p, X[i] * d[i][u])
              || !mul_safe(X[i], d[i][u])
              || p - X[i] * d[i][u] < 0
              || !sub_safe(q[i], d[i][u])
              || q[i] - d[i][u] < 0
              (besides auth)
          */
          /* reverts if not root */
          if iszero(eq(caller, sload(0))) { revert(0, 0) }
          
          /* set c[i][u] = 0 */
          sstore(hash3(1, calldataload(4), calldataload(36)), 0)
          
          /* let d_ = d[i][u] */
          let hash_0 := hash3(3, calldataload(4), calldataload(36))
          let d_ := sload(hash_0)
          
          /* let p_ = p - X[i] * d_ */
          let p_ :=
                isub(sload(9), imul(sload(hash2(5, calldataload(4))), d_))
          
          /* iff p_ >= 0 */
          if slt(p_, 0) { revert(0, 0) }
          
          /* set p = p_ */
          sstore(9, p_)
          
          /* set d[i][u] = 0 */
          sstore(hash_0, 0)
          
          /* let q_i = q[i] - d_ */
          hash_0 := hash2(7, calldataload(4))
          let q_i := isub(sload(hash_0), d_)
          
          /* iff q_i >= 0 */
          if slt(q_i, 0) { revert(0, 0) }
          
          /* set q[i] = q_i */
          sstore(hash_0, q_i)
          stop()
        }
      }
      if eq(sig, 0xf3436159 /* fold(bytes32,int256) */) {
        /*
          reverts iff
            || !add_safe(X[i], delta_X)
            || X[i] + delta_X < 0
            || !mul_safe(delta_X, q[i])
            || !add_safe(V, delta_X * q[i])
            || V + delta_X * q[i] < 0
            || !add_safe(p, delta_X * q[i])
            || p + delta_X * q[i] < 0
            (besides auth)
        */
        /* reverts if not root */
        if iszero(eq(caller, sload(0))) { revert(0, 0) }
        
        /* let X_i = X[i] + delta_X */
        let hash_0 := hash2(5, calldataload(4))
        let X_i := iadd(sload(hash_0), calldataload(36))
        
        /* iff X_i >= 0 */
        if slt(X_i, 0) { revert(0, 0) }
        
        /* set X[i] = X_i */
        sstore(hash_0, X_i)
        
        /* let delta_X_q_i = delta_X * q[i] */
        let delta_X_q_i :=
              imul(calldataload(36), sload(hash2(7, calldataload(4))))
        
        /* let V_ = V + delta_X_q_i */
        let V_ := iadd(sload(11), delta_X_q_i)
        
        /* iff V_ >= 0 */
        if slt(V_, 0) { revert(0, 0) }
        
        /* set V = V_ */
        sstore(11, V_)
        
        /* let p_ = p + delta_X_q_i */
        let p_ := iadd(sload(9), delta_X_q_i)
        
        /* iff p_ >= 0 */
        if slt(p_, 0) { revert(0, 0) }
        
        /* set p = p_ */
        sstore(9, p_)
        stop()
      }
      revert(0, 0)
      function hash2(b, i) -> h {
        mstore(0, b)
        mstore(32, i)
        h := keccak256(0, 64)
      }
      function hash3(b, i, j) -> h {
        mstore(0, b)
        mstore(32, i)
        mstore(0, keccak256(0, 64))
        mstore(32, j)
        h := keccak256(0, 64)
      }
      function imul(x, y) -> z {
        z := mul(x, y)
         if or(and(slt(y, 0), eq(x, 0x8000000000000000000000000000000000000000000000000000000000000000)),
               iszero(or(iszero(y), eq(sdiv(z, y), x)))) { revert(0, 0) }
      }
      function iadd(x, y) -> z {
        z := add(x, y)
        if or(and(sgt(y, 0), iszero(sgt(z, x))),
              and(slt(y, 0), iszero(slt(z, x)))) { revert(0, 0) }
      }
      function isub(x, y) -> z {
        z := sub(x, y)
        if or(and(sgt(y, 0), iszero(sgt(x, z))),
              and(slt(y, 0), iszero(slt(x, z)))) { revert(0, 0) }
      }
    }
  }
}
