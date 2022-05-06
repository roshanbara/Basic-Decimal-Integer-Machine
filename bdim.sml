exception DivisionbyZeroError;
exception ModbyZeroError;
exception InvalidIndexError;

val maxMemSize = 1000;
(* mem is an array *)
val mem = Array.array(maxMemSize,0);

(* Perform Input Operations *)
fun inputop (tgt) = 
    let 
        fun usrinput c = Option.valOf (TextIO.inputLine(TextIO.stdIn));
        fun stoiusr s = Option.valOf (Int.fromString (String.substring (s, 0, (String.size(s) - 1))));
        val x1 = TextIO.print("input: ");
        val x2 = Array.update(mem,tgt,stoiusr(usrinput(1)))
    in 
        ()
    end;
    
(* Assigns mem value *)
fun valassignop (val1,tgt) = Array.update(mem,tgt,val1);

(* Performs Negation operation *)
fun boolop0 (val1,tgt) = Array.update(mem,tgt,if (val1<>0) then 0 else 1);

(* Performs OR and AND operation *)
fun boolop1 (4,val1,val2,tgt) = Array.update(mem,tgt,if (val1<>0 orelse val2<>0) then 1 else 0)
    | boolop1 (_,val1,val2,tgt) = Array.update(mem,tgt,if (val1<>0 andalso val2<>0) then 1 else 0);

(* Performs arithmetic operations *)
fun arithop (6,val1,val2,tgt) = Array.update(mem,tgt,val1+val2)
    | arithop(7,val1,val2,tgt) = Array.update(mem,tgt,val1-val2)
    | arithop(8,val1,val2,tgt) = Array.update(mem,tgt,val1*val2)
    | arithop(9,val1,val2,tgt) = if (val2 = 0) then raise DivisionbyZeroError else Array.update(mem,tgt,val1 div val2)
    | arithop(_,val1,val2,tgt) = if (val2 = 0) then raise ModbyZeroError else Array.update(mem,tgt,val1 mod val2);

(* Evaluates EQUAL AND GREATER THAN operation *)
fun boolop2 (11,val1,val2,tgt) = Array.update(mem,tgt,if val1 = val2 then 1 else 0)
    | boolop2 (_,val1,val2,tgt) = Array.update(mem,tgt,if val1 > val2 then 1 else 0);

(* Printer Function *)
fun printop (val1) = TextIO.print(String.toString(val1)^"\n");

(* Assigns constant *)
fun constassignop (val1,tgt) = Array.update(mem,tgt,val1);

(* Access the ements of tuple *)
fun getval1(codeline: int*int*int*int) = #1 codeline;
fun getval2(codeline: int*int*int*int) = #2 codeline;
fun getval3(codeline: int*int*int*int) = #3 codeline;
fun getval4(codeline: int*int*int*int) = #4 codeline;

(* Determines which line to goto *)
fun linecontrol (codeline,13,n) = if (Array.sub(mem,getval2(codeline))=0) then n+1 else getval4(codeline)
    | linecontrol (codeline,14,n) = getval4(codeline)
    | linecontrol (codeline,_,n) = n+1;


(* Checks Mem Index *)
fun checkindex codeline = 
    let 
        val opc = getval1(codeline);
        val i = getval2(codeline);
        val j = getval3(codeline);
        val k = getval4(codeline);
        fun checkmem (indc:int) = if(indc<0 orelse indc>=maxMemSize) then raise InvalidIndexError else ();
        val p1 = if ((opc>=2 andalso opc<=13) orelse opc=15) then checkmem(i) else ();
        val p2 = if (opc>=4 andalso opc<=12) then checkmem(j) else ();
        val p3 = if ((opc>=1 andalso opc<=12) orelse opc=16) then checkmem(k) else ();
    in
        ()
    end;

(* Assigns operations based on operation code *)
fun exec2 (codeline,opc:int) =
    let
        val z1 = if (opc = 1) then inputop((getval4(codeline)))
        else if (opc = 2) then valassignop(Array.sub(mem,getval2(codeline)),getval4(codeline))
        else if (opc = 3) then boolop0(Array.sub(mem,getval2(codeline)),getval4(codeline))
        else if (opc = 4 orelse opc = 5) then boolop1(opc,Array.sub(mem,getval2(codeline)),Array.sub(mem,getval3(codeline)),getval4(codeline))
        else if (opc >= 6 andalso opc<=10) then arithop(opc,Array.sub(mem,getval2(codeline)),Array.sub(mem,getval3(codeline)),getval4(codeline))
        else if (opc = 11 orelse opc = 12) then boolop2(opc,Array.sub(mem,getval2(codeline)),Array.sub(mem,getval3(codeline)),getval4(codeline))
        else if(opc = 15) then printop(Int.toString(Array.sub(mem,getval2(codeline))))
        else if(opc = 16) then constassignop(getval2(codeline),getval4(codeline))
        else ()
    in
        ()
    end;

(* Executes a codeline *)
fun execprog (code,n) =
    let 
        fun hlpr s = 
            let 
                val codeline = Vector.sub (code,n);
                val x0 = checkindex(codeline);
                val x1 = exec2 (codeline,getval1(codeline));
                val x2 = execprog(code,linecontrol(codeline,getval1(codeline),n))
            in 
                ()
            end;
        
        fun hlpr2 s = if(s = 0) then () else hlpr (1);
        
        val tmp1 = if n<Vector.length (code) then hlpr2 (getval1(Vector.sub (code,n))) else ()
    in ()
    end;
    
(* Driver Function *)
fun interpret s = 
    let
        val progfile = TextIO.openIn s;
        val progtext = TextIO.inputAll progfile;
        val _ = TextIO.closeIn progfile;
        fun checknl (#"\n") = true | checknl c = false;
        (* precode is a vector of string token which have been split using newline character *)
        val precode = Vector.fromList (String.tokens checknl progtext);
        fun prcess s = List.map (Option.valOf o Int.fromString) (String.tokens Char.isPunct (String.substring(s, 1, String.size(s)-2)));
        val codeans1 = Vector.map (prcess) precode
        fun overhaul s = 
            let 
                val v1 = (List.sub(s,0),List.sub(s,1),List.sub(s,2),List.sub(s,3))
            in 
                v1
            end;
        (* Code is a vector of quadruples *)
        val code = Vector.map (overhaul) codeans1;
    in
        execprog(code, 0)
    end;
    