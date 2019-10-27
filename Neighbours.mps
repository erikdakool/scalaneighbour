<?xml version="1.0" encoding="UTF-8"?>
<model ref="r:f48a3a68-fad6-4b73-bb5a-55386d289794(Neighbours)">
  <persistence version="9" />
  <languages>
    <use id="b4fcd96f-fa06-46a4-9520-beaeb32aa9a1" name="ActOne" version="0" />
  </languages>
  <imports>
    <import index="zaie" ref="r:2beb8917-7e4c-43c9-b7bd-08fe08e3aed6(ActOne.Predefined)" implicit="true" />
  </imports>
  <registry>
    <language id="ceab5195-25ea-4f22-9b92-103b95ca8c0c" name="jetbrains.mps.lang.core">
      <concept id="1169194658468" name="jetbrains.mps.lang.core.structure.INamedConcept" flags="ng" index="TrEIO">
        <property id="1169194664001" name="name" index="TrG5h" />
      </concept>
    </language>
    <language id="b4fcd96f-fa06-46a4-9520-beaeb32aa9a1" name="ActOne">
      <concept id="5276604095072500071" name="ActOne.structure.Variable" flags="ng" index="2a4bCg">
        <child id="5276604095072615290" name="ref" index="2a5Jwd" />
      </concept>
      <concept id="5276604095072592121" name="ActOne.structure.Axiom" flags="ng" index="2a5yee">
        <child id="5276604095072592132" name="rightTerm" index="2a5y9N" />
        <child id="5276604095072592129" name="leftTerm" index="2a5y9Q" />
      </concept>
      <concept id="8445081754001449717" name="ActOne.structure.Sort" flags="ng" index="bklvD" />
      <concept id="8445081754001417286" name="ActOne.structure.Specification" flags="ng" index="bkt5q">
        <child id="8445081754001526283" name="sorts" index="blzGn" />
        <child id="6300386900480986014" name="variables" index="pRJcv" />
        <child id="6300386900480908542" name="axioms" index="pRMhZ" />
        <child id="6300386900480909287" name="operators" index="pRMtA" />
        <child id="6849908093511409942" name="actions" index="YRu88" />
        <child id="6816510776893429603" name="imports" index="3QKWGc" />
      </concept>
      <concept id="8445081754001646503" name="ActOne.structure.SortReference" flags="ng" index="bl5qV">
        <reference id="8445081754001646504" name="reference" index="bl5qO" />
      </concept>
      <concept id="6300386900481288729" name="ActOne.structure.OperatorReference" flags="ng" index="pQlao">
        <property id="5944539236828360693" name="vertical" index="3IYp45" />
        <reference id="6300386900481288730" name="ref" index="pQlar" />
        <child id="7054597004572770024" name="parameters" index="EZGxL" />
      </concept>
      <concept id="6300386900480985806" name="ActOne.structure.Operator" flags="ng" index="pRJ9f">
        <property id="6300386900480985810" name="isConstructor" index="pRJ9j" />
        <child id="6300386900480985813" name="parameters" index="pRJ9k" />
        <child id="6300386900480985816" name="output" index="pRJ9p" />
      </concept>
      <concept id="6849908093511409885" name="ActOne.structure.Reduce" flags="ng" index="YRuf3">
        <property id="1955001717827324002" name="isExhaustive" index="1lgsvm" />
        <property id="1955001717825840243" name="steps" index="1lq2J7" />
        <child id="6849908093511409912" name="termToReduce" index="YRufA" />
      </concept>
      <concept id="7333886121707328801" name="ActOne.structure.Let" flags="ng" index="1mjQuN">
        <child id="7333886121707328825" name="storeTerm" index="1mjQuF" />
      </concept>
      <concept id="7333886121707328804" name="ActOne.structure.LetReference" flags="ng" index="1mjQuQ">
        <reference id="7333886121707328805" name="reference" index="1mjQuR" />
      </concept>
      <concept id="7911722725550616649" name="ActOne.structure.VariableReference" flags="ng" index="3O66mJ">
        <reference id="7911722725550616650" name="variableRef" index="3O66mG" />
      </concept>
      <concept id="6816510776893429610" name="ActOne.structure.SpecificationReference" flags="ng" index="3QKWG5">
        <reference id="6816510776893429611" name="reference" index="3QKWG4" />
      </concept>
    </language>
  </registry>
  <node concept="bkt5q" id="7YAKJ4Pj9wW">
    <property role="TrG5h" value="NEIGHBOUR" />
    <node concept="1mjQuN" id="1eGLfpqeC$x" role="YRu88">
      <property role="TrG5h" value="correctboard1" />
      <node concept="pQlao" id="1eGLfpqeDNO" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqeDNV" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqeEqp" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeFkg" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeFUY" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeGxM" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeHs2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeImq" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeJh9" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeKct" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqeKO0" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeKO1" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeKO2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeKO3" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeKO4" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeKO5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeKO6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeLsM" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqeLsN" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeLsO" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeLsP" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeLsQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeLsR" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeLsS" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeLsT" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeM6U" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqeM6V" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeM6W" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeM6X" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeM6Y" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqeM6Z" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeM70" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeM71" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeMMe" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqeNsX" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeOss" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeP6U" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqePLB" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeQMp" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqeQMq" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeQMr" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeQMs" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeQMt" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeRvl" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqeRvm" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeRvn" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeRvo" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqeRvp" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfyRG" role="YRu88">
      <property role="TrG5h" value="correctboard2" />
      <node concept="pQlao" id="1eGLfpqf$r8" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$r9" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$ra" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rb" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rc" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rd" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$re" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rf" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rg" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$rh" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$ri" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rj" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rk" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rl" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rm" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rn" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$ro" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$rp" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$rq" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rr" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rs" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rt" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$ru" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rv" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rw" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$rx" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$ry" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rz" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$r$" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$r_" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rA" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rB" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rC" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$rD" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$rE" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rF" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rG" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$rI" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$rJ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rK" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rL" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rM" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$rN" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$rO" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rP" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$rR" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfyTv" role="YRu88">
      <property role="TrG5h" value="correctboard3" />
      <node concept="pQlao" id="1eGLfpqf$vA" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$vB" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$vC" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vD" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vE" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vF" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vG" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vI" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$vJ" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$vK" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vL" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vM" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vN" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vO" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vP" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$vR" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$vS" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vT" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vU" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vV" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vW" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vX" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$vY" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$vZ" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$w0" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w1" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w3" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w4" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$w7" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$w8" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$w9" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wa" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wb" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$wc" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$wd" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$we" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wf" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wg" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$wh" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$wi" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wj" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wk" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$wl" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfyVm" role="YRu88">
      <property role="TrG5h" value="correctboard4" />
      <node concept="pQlao" id="1eGLfpqf$$4" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$$5" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$$6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$7" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$8" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$9" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$a" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$b" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$c" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$$d" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$$e" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$f" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$g" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$h" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$i" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$j" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$k" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$$l" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$$m" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$n" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$o" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$p" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$q" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$r" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$s" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$$t" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$$u" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$v" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$w" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$x" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$y" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$z" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$$" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$$_" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$$A" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$B" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$C" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$D" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$$E" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$$F" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$G" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$H" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$I" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$$J" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$$K" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$L" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$M" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$$N" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfyXh" role="YRu88">
      <property role="TrG5h" value="incorrectboard1" />
      <node concept="pQlao" id="1eGLfpqf$Cy" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$Cz" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$C$" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$C_" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CA" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CB" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CC" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CD" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CE" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$CF" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$CG" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CI" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CJ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CK" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CL" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CM" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$CN" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$CO" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CP" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CR" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CS" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CT" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CU" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$CV" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$CW" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CX" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CY" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$CZ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$D0" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$D1" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$D2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$D3" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$D4" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$D5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$D6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$D7" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$D8" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$D9" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Da" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Db" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Dc" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Dd" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$De" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Df" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Dg" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Dh" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfyZg" role="YRu88">
      <property role="TrG5h" value="incorrectboard2" />
      <node concept="pQlao" id="1eGLfpqf$H0" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$H1" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$H2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H3" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H4" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H7" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H8" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$H9" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Ha" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hb" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hc" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hd" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$He" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hf" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hg" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Hh" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Hi" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hj" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hk" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hl" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hm" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hn" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Ho" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Hp" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Hq" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hr" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hs" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Ht" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hu" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hv" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hw" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Hx" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$Hy" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Hz" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H$" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$H_" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$HA" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$HB" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$HC" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$HD" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$HE" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$HF" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$HG" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$HH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$HI" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$HJ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfz1j" role="YRu88">
      <property role="TrG5h" value="incorrectboard3" />
      <node concept="pQlao" id="1eGLfpqf$Lu" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$Lv" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Lw" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Lx" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Ly" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Lz" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$L$" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$L_" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LA" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$LB" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$LC" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LD" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LE" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LF" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LG" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LI" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$LJ" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$LK" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LL" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LM" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LN" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LO" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LP" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$LR" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$LS" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LT" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LU" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LV" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LW" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LX" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$LY" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$LZ" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$M0" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$M1" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$M2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$M3" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$M4" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$M5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$M6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$M7" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$M8" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$M9" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$Ma" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Mb" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Mc" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Md" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="1mjQuN" id="1eGLfpqfz3q" role="YRu88">
      <property role="TrG5h" value="incorrectboard4" />
      <node concept="pQlao" id="1eGLfpqf$PW" role="1mjQuF">
        <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
        <node concept="pQlao" id="1eGLfpqf$PX" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$PY" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$PZ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q0" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q1" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q2" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q3" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q4" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Q5" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Q6" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q7" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q8" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q9" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qa" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qb" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qc" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Qd" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Qe" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qf" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qg" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qh" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qi" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qj" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qk" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Ql" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="pQlao" id="1eGLfpqf$Qm" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qn" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qo" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qp" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qq" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qr" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qs" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Qt" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$Qu" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qv" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qw" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Qx" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$Qy" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$Qz" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q$" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$Q_" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$QA" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf$QB" role="EZGxL">
          <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
          <node concept="pQlao" id="1eGLfpqf$QC" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$QD" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$QE" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
          <node concept="pQlao" id="1eGLfpqf$QF" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
          </node>
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqfvLH" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqfvMB" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqfvMM" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqeC$x" resolve="correctboard1" />
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqfGnf" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqfGtz" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqfHcQ" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqfyRG" resolve="correctboard2" />
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqfQg0" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqfQmn" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqfQmy" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqfyXh" resolve="incorrectboard1" />
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqfZri" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqfZxG" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqfZxN" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqfyZg" resolve="incorrectboard2" />
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqgdOl" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqgdUM" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqgdUX" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqfyTv" resolve="correctboard3" />
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqgmjl" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqgmpV" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqgmq5" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqfyVm" resolve="correctboard4" />
        </node>
      </node>
    </node>
    <node concept="YRuf3" id="1eGLfpqgyrx" role="YRu88">
      <property role="1lq2J7" value="0" />
      <property role="1lgsvm" value="true" />
      <node concept="pQlao" id="1eGLfpqgyy4" role="YRufA">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="1mjQuQ" id="1eGLfpqgyyf" role="EZGxL">
          <ref role="1mjQuR" node="1eGLfpqfz1j" resolve="incorrectboard3" />
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvPqZ" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvPsI" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="3O66mJ" id="3J_wpcuvPsW" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuvPFB" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvPUl" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="7ZST_MPV00H" role="pRMhZ">
      <node concept="pQlao" id="7ZST_MPV03$" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="7ZST_MPV03M" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
        <node concept="3O66mJ" id="7ZST_MPV0Ca" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
        </node>
      </node>
      <node concept="pQlao" id="7ZST_MPV10s" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="7ZST_MPV1oO" role="pRMhZ">
      <node concept="pQlao" id="7ZST_MPV1rK" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="3O66mJ" id="7ZST_MPV1Ou" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
        </node>
        <node concept="pQlao" id="7ZST_MPV3zw" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
      </node>
      <node concept="pQlao" id="7ZST_MPV3Wh" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvu8G" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuiz" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
      <node concept="pQlao" id="3J_wpcuvub1" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvufw" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
        </node>
        <node concept="pQlao" id="3J_wpcuvug2" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuwz" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuwX" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuxb" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
        </node>
        <node concept="pQlao" id="3J_wpcuvu_X" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuDi" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuJP" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuJQ" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuJR" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuJS" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuJT" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvulb" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvulw" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvulI" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
        </node>
        <node concept="pQlao" id="3J_wpcuvupW" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvusT" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuK_" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuKA" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuKB" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuKC" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuKD" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuLq" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuLr" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuLs" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuLt" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuLu" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuMk" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuMl" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuMm" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuMn" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuMo" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuNj" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuNk" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuNl" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuNm" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuNn" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuOn" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuOo" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuOp" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuOq" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuOr" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvuPw" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvuPx" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvuPy" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
        </node>
        <node concept="pQlao" id="3J_wpcuvuPz" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvolK" resolve="ONE" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvuP$" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvv_l" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvv_m" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvv_n" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
        </node>
        <node concept="pQlao" id="3J_wpcuvv_o" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomg" resolve="TWO" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvv_p" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvvAC" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvvAD" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="pQlao" id="3J_wpcuvvAE" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomy" resolve="FOUR" />
        </node>
        <node concept="pQlao" id="3J_wpcuvvAF" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomp" resolve="THREE" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvvAG" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvCBw" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvCCI" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvtOC" resolve="NEIGHBOUR" />
      </node>
      <node concept="pQlao" id="3J_wpcuvCCW" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvD$h" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvDAg" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvD_y" resolve="NOTNEIGHBOUR" />
      </node>
      <node concept="pQlao" id="3J_wpcuvDAu" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="7ZST_MPVb2r" role="pRMhZ">
      <node concept="pQlao" id="7ZST_MPVb5s" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
        <node concept="pQlao" id="7ZST_MPVb5E" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
        <node concept="3O66mJ" id="7ZST_MPVbuH" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
        </node>
        <node concept="3O66mJ" id="7ZST_MPVc4t" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
        </node>
      </node>
      <node concept="pQlao" id="7ZST_MPVctK" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="7ZST_MPVcR8" role="pRMhZ">
      <node concept="pQlao" id="7ZST_MPVcUf" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
        <node concept="3O66mJ" id="7ZST_MPVcUt" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
        </node>
        <node concept="pQlao" id="7ZST_MPVdk3" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
        <node concept="3O66mJ" id="7ZST_MPVdHS" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
        </node>
      </node>
      <node concept="pQlao" id="7ZST_MPVe7J" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuv_7m" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuv_QO" role="2a5y9N">
        <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
        <node concept="3O66mJ" id="3J_wpcuvAbS" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuvAoH" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvCQJ" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
        <node concept="3O66mJ" id="3J_wpcuvCZF" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuvDdb" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
        </node>
        <node concept="pQlao" id="3J_wpcuvEk_" role="EZGxL">
          <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvDAM" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvDC6" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
        <node concept="3O66mJ" id="3J_wpcuvDCk" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuvDQK" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
        </node>
        <node concept="pQlao" id="3J_wpcuvECE" role="EZGxL">
          <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvEMK" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwP" resolve="Not" />
        <node concept="pQlao" id="3J_wpcuvEWU" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvu9a" resolve="withinOne" />
          <node concept="3O66mJ" id="3J_wpcuvEXl" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
          </node>
          <node concept="3O66mJ" id="3J_wpcuvF7S" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
          </node>
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuwrv5" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuwrxr" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
        <node concept="3O66mJ" id="3J_wpcuwrxD" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
        <node concept="pQlao" id="3J_wpcuwswJ" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuwsPS" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuwtb4" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuwtdv" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
        <node concept="pQlao" id="3J_wpcuwtdH" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuwtHR" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuwu3p" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuwup1" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuwurx" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
        <node concept="pQlao" id="3J_wpcuwurJ" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
        <node concept="pQlao" id="3J_wpcuwuWz" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvomJ" resolve="UNSOLVED" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuwvty" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvX$o" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvXAk" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
        <node concept="3O66mJ" id="3J_wpcuvXAy" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuvXB3" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuvXRp" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jwF" resolve="False" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuw0sb" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuw0uc" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
        <node concept="3O66mJ" id="3J_wpcuw0uq" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
        </node>
        <node concept="3O66mJ" id="3J_wpcuw0Sz" role="EZGxL">
          <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuw1a5" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5Oc3jw_" resolve="True" />
      </node>
    </node>
    <node concept="2a5yee" id="3J_wpcuvW9L" role="pRMhZ">
      <node concept="pQlao" id="3J_wpcuvWcu" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
        <node concept="pQlao" id="3J_wpcuvWd5" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
          <node concept="3O66mJ" id="3J_wpcuvWd$" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
          </node>
          <node concept="3O66mJ" id="3J_wpcuvWsU" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
          </node>
          <node concept="3O66mJ" id="3J_wpcuvWGn" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
          </node>
          <node concept="3O66mJ" id="3J_wpcuvX3J" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
          </node>
          <node concept="3O66mJ" id="5lTx$HLjq2z" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
          </node>
          <node concept="3O66mJ" id="5lTx$HLjqFq" role="EZGxL">
            <ref role="3O66mG" node="5lTx$HLjqZR" resolve="n2" />
          </node>
          <node concept="3O66mJ" id="5lTx$HLjroV" role="EZGxL">
            <ref role="3O66mG" node="5lTx$HLjr0U" resolve="n3" />
          </node>
        </node>
      </node>
      <node concept="pQlao" id="3J_wpcuw65m" role="2a5y9N">
        <property role="3IYp45" value="true" />
        <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
        <node concept="pQlao" id="3J_wpcuw6n0" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
          <node concept="3O66mJ" id="3J_wpcuw6CI" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
          </node>
          <node concept="3O66mJ" id="3J_wpcuw6UD" role="EZGxL">
            <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
          </node>
        </node>
        <node concept="pQlao" id="3J_wpcuw7lP" role="EZGxL">
          <property role="3IYp45" value="true" />
          <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
          <node concept="pQlao" id="3J_wpcuw7Cc" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
            <node concept="3O66mJ" id="3J_wpcuw7Uy" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
            </node>
            <node concept="3O66mJ" id="3J_wpcuw8mh" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
            </node>
          </node>
          <node concept="pQlao" id="3J_wpcuw8Ml" role="EZGxL">
            <property role="3IYp45" value="true" />
            <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
            <node concept="pQlao" id="3J_wpcuw95c" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
              <node concept="3O66mJ" id="3J_wpcuw9o2" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
              </node>
              <node concept="3O66mJ" id="3J_wpcuw9Ox" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
              </node>
            </node>
            <node concept="pQlao" id="3J_wpcuwaqV" role="EZGxL">
              <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
              <node concept="pQlao" id="3J_wpcuwaIi" role="EZGxL">
                <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
                <node concept="3O66mJ" id="3J_wpcuwb1C" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
                </node>
                <node concept="3O66mJ" id="3J_wpcuwblb" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
                </node>
              </node>
              <node concept="pQlao" id="3J_wpcuwdXe" role="EZGxL">
                <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
                <node concept="pQlao" id="3J_wpcuweh5" role="EZGxL">
                  <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
                  <node concept="3O66mJ" id="3J_wpcuwe$V" role="EZGxL">
                    <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
                  </node>
                  <node concept="3O66mJ" id="3J_wpcuweSY" role="EZGxL">
                    <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
                  </node>
                </node>
                <node concept="pQlao" id="5lTx$HLjt0x" role="EZGxL">
                  <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
                  <node concept="pQlao" id="5lTx$HLjtDS" role="EZGxL">
                    <ref role="pQlar" node="3J_wpcuvXzj" resolve="uniqueValue" />
                    <node concept="3O66mJ" id="5lTx$HLjuje" role="EZGxL">
                      <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
                    </node>
                    <node concept="3O66mJ" id="5lTx$HLjvht" role="EZGxL">
                      <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
                    </node>
                  </node>
                  <node concept="pQlao" id="5lTx$HLjwg1" role="EZGxL">
                    <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
                    <node concept="pQlao" id="5lTx$HLjwTS" role="EZGxL">
                      <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
                      <node concept="3O66mJ" id="5lTx$HLjxzI" role="EZGxL">
                        <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
                      </node>
                      <node concept="3O66mJ" id="5lTx$HLjydG" role="EZGxL">
                        <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
                      </node>
                      <node concept="3O66mJ" id="5lTx$HLjyRS" role="EZGxL">
                        <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
                      </node>
                    </node>
                    <node concept="pQlao" id="5lTx$HLjBR6" role="EZGxL">
                      <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
                      <node concept="pQlao" id="5lTx$HLjCxA" role="EZGxL">
                        <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
                        <node concept="3O66mJ" id="5lTx$HLjDc4" role="EZGxL">
                          <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
                        </node>
                        <node concept="3O66mJ" id="5lTx$HLjFbQ" role="EZGxL">
                          <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
                        </node>
                        <node concept="3O66mJ" id="5lTx$HLjFQE" role="EZGxL">
                          <ref role="3O66mG" node="5lTx$HLjqZR" resolve="n2" />
                        </node>
                      </node>
                      <node concept="pQlao" id="5lTx$HLjIdl" role="EZGxL">
                        <ref role="pQlar" node="3J_wpcuvoqp" resolve="checkNeighbours" />
                        <node concept="3O66mJ" id="5lTx$HLjISt" role="EZGxL">
                          <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
                        </node>
                        <node concept="3O66mJ" id="5lTx$HLjJT9" role="EZGxL">
                          <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
                        </node>
                        <node concept="3O66mJ" id="5lTx$HLjKU5" role="EZGxL">
                          <ref role="3O66mG" node="5lTx$HLjr0U" resolve="n3" />
                        </node>
                      </node>
                    </node>
                  </node>
                </node>
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="7ZST_MPVglj" role="pRMhZ">
      <node concept="pQlao" id="7ZST_MPVgok" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuvW8W" resolve="checkHorizontal" />
        <node concept="pQlao" id="1eGLfpqf7Yt" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
          <node concept="pQlao" id="1eGLfpqf7Yu" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqf7Yv" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Yw" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Yx" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Yy" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Yz" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Y$" role="EZGxL">
              <ref role="3O66mG" node="5lTx$HLjqZR" resolve="n2" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Y_" role="EZGxL">
              <ref role="3O66mG" node="5lTx$HLjr0U" resolve="n3" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf7YA" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqf7YB" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YC" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YD" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvAOi" resolve="sq23" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YE" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvAOx" resolve="sq24" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YF" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdGYX" resolve="n4" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YG" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmL" resolve="n5" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YH" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmN" resolve="n6" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf7YI" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqf7YJ" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcr9" resolve="sq31" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YK" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrb" resolve="sq32" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YL" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrd" resolve="sq33" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YM" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrf" resolve="sq34" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YN" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmP" resolve="n7" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YO" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmR" resolve="n8" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YP" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVnX" resolve="n9" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf7YQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqf7YR" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrV" resolve="sq41" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YS" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrX" resolve="sq42" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YT" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrZ" resolve="sq43" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YU" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcs1" resolve="sq44" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YV" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVnZ" resolve="n10" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YW" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVo1" resolve="n11" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7YX" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVo3" resolve="n12" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf7YY" role="EZGxL">
            <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
            <node concept="3O66mJ" id="1eGLfpqf7YZ" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVph" resolve="n13" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Z0" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVpj" resolve="n14" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Z1" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVpl" resolve="n15" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Z2" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVpn" resolve="n16" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf7Z3" role="EZGxL">
            <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
            <node concept="3O66mJ" id="1eGLfpqf7Z4" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqH" resolve="n17" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Z5" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqJ" resolve="n18" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Z6" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqL" resolve="n19" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Z7" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqN" resolve="n20" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf7Z8" role="EZGxL">
            <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
            <node concept="3O66mJ" id="1eGLfpqf7Z9" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsh" resolve="n21" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Za" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsj" resolve="n22" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Zb" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsl" resolve="n23" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf7Zc" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsn" resolve="n24" />
            </node>
          </node>
        </node>
      </node>
      <node concept="pQlao" id="1eGLfpqf9TF" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
        <node concept="pQlao" id="1eGLfpqf9TG" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
          <node concept="pQlao" id="1eGLfpqf9TH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqf9TI" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf9TJ" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf9TK" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf9TL" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf9TM" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf9TN" role="EZGxL">
              <ref role="3O66mG" node="5lTx$HLjqZR" resolve="n2" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqf9TO" role="EZGxL">
              <ref role="3O66mG" node="5lTx$HLjr0U" resolve="n3" />
            </node>
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqf9TP" role="EZGxL">
          <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
          <node concept="pQlao" id="1eGLfpqf9TQ" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
            <node concept="pQlao" id="1eGLfpqf9TR" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
              <node concept="3O66mJ" id="1eGLfpqf9TS" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqf9TT" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqf9TU" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuvAOi" resolve="sq23" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqf9TV" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuvAOx" resolve="sq24" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqf9TW" role="EZGxL">
                <ref role="3O66mG" node="1eGLfpqdGYX" resolve="n4" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqf9TX" role="EZGxL">
                <ref role="3O66mG" node="1eGLfpqdVmL" resolve="n5" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqf9TY" role="EZGxL">
                <ref role="3O66mG" node="1eGLfpqdVmN" resolve="n6" />
              </node>
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqf9TZ" role="EZGxL">
            <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
            <node concept="pQlao" id="1eGLfpqf9U0" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
              <node concept="pQlao" id="1eGLfpqf9U1" role="EZGxL">
                <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
                <node concept="3O66mJ" id="1eGLfpqf9U2" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcr9" resolve="sq31" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9U3" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrb" resolve="sq32" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9U4" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrd" resolve="sq33" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9U5" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrf" resolve="sq34" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9U6" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVmP" resolve="n7" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9U7" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVmR" resolve="n8" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9U8" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVnX" resolve="n9" />
                </node>
              </node>
            </node>
            <node concept="pQlao" id="1eGLfpqf9U9" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
              <node concept="pQlao" id="1eGLfpqf9Ua" role="EZGxL">
                <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
                <node concept="3O66mJ" id="1eGLfpqf9Ub" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrV" resolve="sq41" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9Uc" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrX" resolve="sq42" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9Ud" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrZ" resolve="sq43" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9Ue" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcs1" resolve="sq44" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9Uf" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVnZ" resolve="n10" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9Ug" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVo1" resolve="n11" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqf9Uh" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVo3" resolve="n12" />
                </node>
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="1eGLfpqdQ6B" role="pRMhZ">
      <node concept="pQlao" id="1eGLfpqdQuM" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuwPgY" resolve="checkVertical" />
        <node concept="pQlao" id="1eGLfpqdQv0" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvon0" resolve="mkBoard" />
          <node concept="pQlao" id="1eGLfpqdQvp" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqdQvV" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdRc5" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdRDE" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdSm5" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdT3i" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvosx" resolve="n1" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdTx9" role="EZGxL">
              <ref role="3O66mG" node="5lTx$HLjqZR" resolve="n2" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUt1" role="EZGxL">
              <ref role="3O66mG" node="5lTx$HLjr0U" resolve="n3" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqdUVH" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqdUVI" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUVJ" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUVK" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvAOi" resolve="sq23" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUVL" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvAOx" resolve="sq24" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUVM" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdGYX" resolve="n4" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUVN" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmL" resolve="n5" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUVO" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmN" resolve="n6" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqdUXh" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqdUXi" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcr9" resolve="sq31" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUXj" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrb" resolve="sq32" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUXk" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrd" resolve="sq33" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUXl" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrf" resolve="sq34" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUXm" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmP" resolve="n7" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUXn" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVmR" resolve="n8" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUXo" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVnX" resolve="n9" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqdUZ5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqdUZ6" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrV" resolve="sq41" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUZ7" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrX" resolve="sq42" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUZ8" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrZ" resolve="sq43" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUZ9" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcs1" resolve="sq44" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUZa" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVnZ" resolve="n10" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUZb" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVo1" resolve="n11" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqdUZc" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVo3" resolve="n12" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqdV19" role="EZGxL">
            <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
            <node concept="3O66mJ" id="1eGLfpqe2Ml" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVph" resolve="n13" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe3_H" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVpj" resolve="n14" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe4Vu" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVpl" resolve="n15" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe5Jg" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVpn" resolve="n16" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqe6iM" role="EZGxL">
            <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
            <node concept="3O66mJ" id="1eGLfpqe6iN" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqH" resolve="n17" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe6iO" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqJ" resolve="n18" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe6iP" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqL" resolve="n19" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe6iQ" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqN" resolve="n20" />
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqe6kV" role="EZGxL">
            <ref role="pQlar" node="1eGLfpqdGT8" resolve="mkVerticalNeighboursLine" />
            <node concept="3O66mJ" id="1eGLfpqe6kW" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsh" resolve="n21" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe6kX" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsj" resolve="n22" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe6kY" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsl" resolve="n23" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqe6kZ" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsn" resolve="n24" />
            </node>
          </node>
        </node>
      </node>
      <node concept="pQlao" id="1eGLfpqeb8j" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
        <node concept="pQlao" id="1eGLfpqebHF" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
          <node concept="pQlao" id="1eGLfpqecj5" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
            <node concept="3O66mJ" id="1eGLfpqecSC" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcbx" resolve="sq11" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqedL4" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuvosh" resolve="sq21" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqeemP" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcr9" resolve="sq31" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqeffy" role="EZGxL">
              <ref role="3O66mG" node="3J_wpcuxcrV" resolve="sq41" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqeg8n" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVph" resolve="n13" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqegIq" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVqH" resolve="n17" />
            </node>
            <node concept="3O66mJ" id="1eGLfpqehUV" role="EZGxL">
              <ref role="3O66mG" node="1eGLfpqdVsh" resolve="n21" />
            </node>
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqeiOS" role="EZGxL">
          <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
          <node concept="pQlao" id="1eGLfpqejrD" role="EZGxL">
            <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
            <node concept="pQlao" id="1eGLfpqejrE" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
              <node concept="3O66mJ" id="1eGLfpqejrF" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuxcbz" resolve="sq12" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqejrG" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuvoso" resolve="sq22" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqejrH" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuxcrb" resolve="sq32" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqejrI" role="EZGxL">
                <ref role="3O66mG" node="3J_wpcuxcrX" resolve="sq42" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqejrJ" role="EZGxL">
                <ref role="3O66mG" node="1eGLfpqdVpj" resolve="n14" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqejrK" role="EZGxL">
                <ref role="3O66mG" node="1eGLfpqdVqJ" resolve="n18" />
              </node>
              <node concept="3O66mJ" id="1eGLfpqejrL" role="EZGxL">
                <ref role="3O66mG" node="1eGLfpqdVsj" resolve="n22" />
              </node>
            </node>
          </node>
          <node concept="pQlao" id="1eGLfpqek3P" role="EZGxL">
            <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
            <node concept="pQlao" id="1eGLfpqekFQ" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
              <node concept="pQlao" id="1eGLfpqekFR" role="EZGxL">
                <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
                <node concept="3O66mJ" id="1eGLfpqekFS" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcb_" resolve="sq13" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqekFT" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuvAOi" resolve="sq23" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqekFU" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrd" resolve="sq33" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqekFV" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrZ" resolve="sq43" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqekFW" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVpl" resolve="n15" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqekFX" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVqL" resolve="n19" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqekFY" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVsl" resolve="n23" />
                </node>
              </node>
            </node>
            <node concept="pQlao" id="1eGLfpqenU$" role="EZGxL">
              <ref role="pQlar" node="3J_wpcuy5M2" resolve="checkLine" />
              <node concept="pQlao" id="1eGLfpqenU_" role="EZGxL">
                <ref role="pQlar" node="3J_wpcuy0Zp" resolve="mkLine" />
                <node concept="3O66mJ" id="1eGLfpqenUA" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcbB" resolve="sq14" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqenUB" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuvAOx" resolve="sq24" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqenUC" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcrf" resolve="sq34" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqenUD" role="EZGxL">
                  <ref role="3O66mG" node="3J_wpcuxcs1" resolve="sq44" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqenUE" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVpn" resolve="n16" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqenUF" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVqN" resolve="n20" />
                </node>
                <node concept="3O66mJ" id="1eGLfpqenUG" role="EZGxL">
                  <ref role="3O66mG" node="1eGLfpqdVsn" resolve="n24" />
                </node>
              </node>
            </node>
          </node>
        </node>
      </node>
    </node>
    <node concept="2a5yee" id="1eGLfpqfpsQ" role="pRMhZ">
      <node concept="pQlao" id="1eGLfpqfpyD" role="2a5y9Q">
        <ref role="pQlar" node="3J_wpcuwy5a" resolve="checkBoard" />
        <node concept="3O66mJ" id="1eGLfpqfqru" role="EZGxL">
          <ref role="3O66mG" node="1eGLfpqfpyR" resolve="board" />
        </node>
      </node>
      <node concept="pQlao" id="1eGLfpqfrfy" role="2a5y9N">
        <ref role="pQlar" to="zaie:34Rb5OcqQZf" resolve="And" />
        <node concept="pQlao" id="1eGLfpqfs3M" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuvW8W" resolve="checkHorizontal" />
          <node concept="3O66mJ" id="1eGLfpqfsS4" role="EZGxL">
            <ref role="3O66mG" node="1eGLfpqfpyR" resolve="board" />
          </node>
        </node>
        <node concept="pQlao" id="1eGLfpqfu6N" role="EZGxL">
          <ref role="pQlar" node="3J_wpcuwPgY" resolve="checkVertical" />
          <node concept="3O66mJ" id="1eGLfpqfuVt" role="EZGxL">
            <ref role="3O66mG" node="1eGLfpqfpyR" resolve="board" />
          </node>
        </node>
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcbx" role="pRJcv">
      <property role="TrG5h" value="sq11" />
      <node concept="bl5qV" id="3J_wpcuxcby" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcbz" role="pRJcv">
      <property role="TrG5h" value="sq12" />
      <node concept="bl5qV" id="3J_wpcuxcb$" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcb_" role="pRJcv">
      <property role="TrG5h" value="sq13" />
      <node concept="bl5qV" id="3J_wpcuxcbA" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcbB" role="pRJcv">
      <property role="TrG5h" value="sq14" />
      <node concept="bl5qV" id="3J_wpcuxcbC" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuvosh" role="pRJcv">
      <property role="TrG5h" value="sq21" />
      <node concept="bl5qV" id="3J_wpcuvosi" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuvoso" role="pRJcv">
      <property role="TrG5h" value="sq22" />
      <node concept="bl5qV" id="3J_wpcuvosp" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuvAOi" role="pRJcv">
      <property role="TrG5h" value="sq23" />
      <node concept="bl5qV" id="3J_wpcuvAOj" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuvAOx" role="pRJcv">
      <property role="TrG5h" value="sq24" />
      <node concept="bl5qV" id="3J_wpcuvAOy" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcr9" role="pRJcv">
      <property role="TrG5h" value="sq31" />
      <node concept="bl5qV" id="3J_wpcuxcra" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcrb" role="pRJcv">
      <property role="TrG5h" value="sq32" />
      <node concept="bl5qV" id="3J_wpcuxcrc" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcrd" role="pRJcv">
      <property role="TrG5h" value="sq33" />
      <node concept="bl5qV" id="3J_wpcuxcre" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcrf" role="pRJcv">
      <property role="TrG5h" value="sq34" />
      <node concept="bl5qV" id="3J_wpcuxcrg" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcrV" role="pRJcv">
      <property role="TrG5h" value="sq41" />
      <node concept="bl5qV" id="3J_wpcuxcrW" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcrX" role="pRJcv">
      <property role="TrG5h" value="sq42" />
      <node concept="bl5qV" id="3J_wpcuxcrY" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcrZ" role="pRJcv">
      <property role="TrG5h" value="sq43" />
      <node concept="bl5qV" id="3J_wpcuxcs0" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuxcs1" role="pRJcv">
      <property role="TrG5h" value="sq44" />
      <node concept="bl5qV" id="3J_wpcuxcs2" role="2a5Jwd">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="2a4bCg" id="3J_wpcuvosx" role="pRJcv">
      <property role="TrG5h" value="n1" />
      <node concept="bl5qV" id="3J_wpcuvosy" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="5lTx$HLjqZR" role="pRJcv">
      <property role="TrG5h" value="n2" />
      <node concept="bl5qV" id="5lTx$HLjqZS" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="5lTx$HLjr0U" role="pRJcv">
      <property role="TrG5h" value="n3" />
      <node concept="bl5qV" id="5lTx$HLjr0V" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdGYX" role="pRJcv">
      <property role="TrG5h" value="n4" />
      <node concept="bl5qV" id="1eGLfpqdGYY" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVmL" role="pRJcv">
      <property role="TrG5h" value="n5" />
      <node concept="bl5qV" id="1eGLfpqdVmM" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVmN" role="pRJcv">
      <property role="TrG5h" value="n6" />
      <node concept="bl5qV" id="1eGLfpqdVmO" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVmP" role="pRJcv">
      <property role="TrG5h" value="n7" />
      <node concept="bl5qV" id="1eGLfpqdVmQ" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVmR" role="pRJcv">
      <property role="TrG5h" value="n8" />
      <node concept="bl5qV" id="1eGLfpqdVmS" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVnX" role="pRJcv">
      <property role="TrG5h" value="n9" />
      <node concept="bl5qV" id="1eGLfpqdVnY" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVnZ" role="pRJcv">
      <property role="TrG5h" value="n10" />
      <node concept="bl5qV" id="1eGLfpqdVo0" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVo1" role="pRJcv">
      <property role="TrG5h" value="n11" />
      <node concept="bl5qV" id="1eGLfpqdVo2" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVo3" role="pRJcv">
      <property role="TrG5h" value="n12" />
      <node concept="bl5qV" id="1eGLfpqdVo4" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVph" role="pRJcv">
      <property role="TrG5h" value="n13" />
      <node concept="bl5qV" id="1eGLfpqdVpi" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVpj" role="pRJcv">
      <property role="TrG5h" value="n14" />
      <node concept="bl5qV" id="1eGLfpqdVpk" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVpl" role="pRJcv">
      <property role="TrG5h" value="n15" />
      <node concept="bl5qV" id="1eGLfpqdVpm" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVpn" role="pRJcv">
      <property role="TrG5h" value="n16" />
      <node concept="bl5qV" id="1eGLfpqdVpo" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVqH" role="pRJcv">
      <property role="TrG5h" value="n17" />
      <node concept="bl5qV" id="1eGLfpqdVqI" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVqJ" role="pRJcv">
      <property role="TrG5h" value="n18" />
      <node concept="bl5qV" id="1eGLfpqdVqK" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVqL" role="pRJcv">
      <property role="TrG5h" value="n19" />
      <node concept="bl5qV" id="1eGLfpqdVqM" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVqN" role="pRJcv">
      <property role="TrG5h" value="n20" />
      <node concept="bl5qV" id="1eGLfpqdVqO" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVsh" role="pRJcv">
      <property role="TrG5h" value="n21" />
      <node concept="bl5qV" id="1eGLfpqdVsi" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVsj" role="pRJcv">
      <property role="TrG5h" value="n22" />
      <node concept="bl5qV" id="1eGLfpqdVsk" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVsl" role="pRJcv">
      <property role="TrG5h" value="n23" />
      <node concept="bl5qV" id="1eGLfpqdVsm" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqdVsn" role="pRJcv">
      <property role="TrG5h" value="n24" />
      <node concept="bl5qV" id="1eGLfpqdVso" role="2a5Jwd">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="2a4bCg" id="1eGLfpqfpyR" role="pRJcv">
      <property role="TrG5h" value="board" />
      <node concept="bl5qV" id="1eGLfpqfpyS" role="2a5Jwd">
        <ref role="bl5qO" node="3J_wpcuvnfK" resolve="Board" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvolK" role="pRMtA">
      <property role="pRJ9j" value="true" />
      <property role="TrG5h" value="ONE" />
      <node concept="bl5qV" id="3J_wpcuvolL" role="pRJ9p">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvomg" role="pRMtA">
      <property role="TrG5h" value="TWO" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvomh" role="pRJ9p">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvomp" role="pRMtA">
      <property role="TrG5h" value="THREE" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvomq" role="pRJ9p">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvomy" role="pRMtA">
      <property role="TrG5h" value="FOUR" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvomz" role="pRJ9p">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvomJ" role="pRMtA">
      <property role="TrG5h" value="UNSOLVED" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvomK" role="pRJ9p">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvtOC" role="pRMtA">
      <property role="TrG5h" value="NEIGHBOUR" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvtOD" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvD_y" role="pRMtA">
      <property role="TrG5h" value="NOTNEIGHBOUR" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvD_z" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuy0Zp" role="pRMtA">
      <property role="TrG5h" value="mkLine" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuy10A" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuy10F" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuy10J" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuy10Q" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="5lTx$HLjosy" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="5lTx$HLjp5f" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="5lTx$HLjp5m" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="3J_wpcuy0Zq" role="pRJ9p">
        <ref role="bl5qO" node="3J_wpcuy0Zf" resolve="Line" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvon0" role="pRMtA">
      <property role="TrG5h" value="mkBoard" />
      <property role="pRJ9j" value="true" />
      <node concept="bl5qV" id="3J_wpcuvonh" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuy0Zf" resolve="Line" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvonm" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuy0Zf" resolve="Line" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvonq" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuy0Zf" resolve="Line" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvonv" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuy0Zf" resolve="Line" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdIrj" role="pRJ9k">
        <ref role="bl5qO" node="1eGLfpqdGUW" resolve="VerticalNeighboursLine" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdIrq" role="pRJ9k">
        <ref role="bl5qO" node="1eGLfpqdGUW" resolve="VerticalNeighboursLine" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdIr$" role="pRJ9k">
        <ref role="bl5qO" node="1eGLfpqdGUW" resolve="VerticalNeighboursLine" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvon1" role="pRJ9p">
        <ref role="bl5qO" node="3J_wpcuvnfK" resolve="Board" />
      </node>
    </node>
    <node concept="pRJ9f" id="1eGLfpqdGT8" role="pRMtA">
      <property role="pRJ9j" value="true" />
      <property role="TrG5h" value="mkVerticalNeighboursLine" />
      <node concept="bl5qV" id="1eGLfpqdGUE" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdGUJ" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdGUN" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdGYD" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="1eGLfpqdGT9" role="pRJ9p">
        <ref role="bl5qO" node="1eGLfpqdGUW" resolve="VerticalNeighboursLine" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvu9a" role="pRMtA">
      <property role="TrG5h" value="withinOne" />
      <node concept="bl5qV" id="3J_wpcuvu9M" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvu9R" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvu9b" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvoqp" role="pRMtA">
      <property role="TrG5h" value="checkNeighbours" />
      <node concept="bl5qV" id="3J_wpcuvoqU" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvoqZ" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvtCS" role="pRJ9k">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvoqq" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvW8W" role="pRMtA">
      <property role="TrG5h" value="checkHorizontal" />
      <node concept="bl5qV" id="3J_wpcuvW9E" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuvnfK" resolve="Board" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvW8X" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuy5M2" role="pRMtA">
      <property role="TrG5h" value="checkLine" />
      <node concept="bl5qV" id="3J_wpcuy5Nl" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuy0Zf" resolve="Line" />
      </node>
      <node concept="bl5qV" id="3J_wpcuy5M3" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuvXzj" role="pRMtA">
      <property role="TrG5h" value="uniqueValue" />
      <node concept="bl5qV" id="3J_wpcuvX$4" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvX$9" role="pRJ9k">
        <ref role="bl5qO" node="7YAKJ4Pj9zb" resolve="Square" />
      </node>
      <node concept="bl5qV" id="3J_wpcuvXzk" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuwy5a" role="pRMtA">
      <property role="TrG5h" value="checkBoard" />
      <node concept="bl5qV" id="3J_wpcuwy5Z" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuvnfK" resolve="Board" />
      </node>
      <node concept="bl5qV" id="3J_wpcuwy5b" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="pRJ9f" id="3J_wpcuwPgY" role="pRMtA">
      <property role="TrG5h" value="checkVertical" />
      <node concept="bl5qV" id="3J_wpcuwPhQ" role="pRJ9k">
        <ref role="bl5qO" node="3J_wpcuvnfK" resolve="Board" />
      </node>
      <node concept="bl5qV" id="3J_wpcuwPgZ" role="pRJ9p">
        <ref role="bl5qO" to="zaie:7zB5IrGSITC" resolve="Bool" />
      </node>
    </node>
    <node concept="bklvD" id="3J_wpcuvnfK" role="blzGn">
      <property role="TrG5h" value="Board" />
    </node>
    <node concept="bklvD" id="7YAKJ4Pj9zb" role="blzGn">
      <property role="TrG5h" value="Square" />
    </node>
    <node concept="bklvD" id="3J_wpcuy0Zf" role="blzGn">
      <property role="TrG5h" value="Line" />
    </node>
    <node concept="bklvD" id="1eGLfpqdGUW" role="blzGn">
      <property role="TrG5h" value="VerticalNeighboursLine" />
    </node>
    <node concept="3QKWG5" id="7YAKJ4Pj9yi" role="3QKWGc">
      <ref role="3QKWG4" to="zaie:7zB5IrGSITB" resolve="BOOLEAN" />
    </node>
  </node>
</model>

