grammar Calculator;
//BC Calculator Implementation
@header 
{
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
}
@parser::members
{
private Map<String, Double> variables = new HashMap<String, Double>();
private void addVariable(String varname, double x1)
        {variables.put(varname, x1);}
private double getVariable(String varname)
        {return variables.get(varname);}
private void replaceVariable(String varname, double x2)
        {variables.replace(varname, x2);}
}
varDef:  ID '=' exprD {addVariable($ID.text, $exprD.i);}
        | ID '=' 'read()' {Scanner scanny = new Scanner(System.in); 
        double userInput = 0.0;
        do{ try {userInput = scanny.nextDouble();addVariable($ID.text, userInput);break;}
            catch (Exception e) {System.out.println("Invalid input, enter a number only");scanny.nextLine();}
        }
        while(true);
        
        
        };

printList: 'print' print ( ',' print)*? ;

print: exprD*? {System.out.print(Double.toString($exprD.i)+ " ");};

exprList: topExpr( ';' topExpr)* ';'? ; 

topExpr:  exprD  { System.out.println("result: "+ Double.toString($exprD.i));}  
         | varDef
         | printList
        ;


    //NEED TO FIX:: Some expressions dont work with spaces
    exprD returns [double i]: 
    ID {$i = getVariable($ID.text);}
    | op='-' e=exprD { $i=((-1)*($e.i));}
    | op='++' ID {double temp = getVariable($ID.text) + 1.0; replaceVariable($ID.text, temp);$i = getVariable($ID.text); }
    | ID op='++' {$i = getVariable($ID.text); double temp = $i + 1.0; replaceVariable($ID.text, temp);}
    | op= '--' ID {double temp = getVariable($ID.text) - 1.0; replaceVariable($ID.text, temp);$i = getVariable($ID.text); }
    | ID op='--' {$i = getVariable($ID.text); double temp = $i - 1.0; replaceVariable($ID.text, temp);}
    | el=exprD op='^' er=exprD { $i=Math.pow($el.i, $er.i);}
    | op='sqrt(' e=exprD')' { if ($e.i < 0 ){ System.out.println("Runtime error: Square root of a negative number");System.exit(1);} else$i=Math.sqrt($e.i);}
    | op='s(' e=exprD')' {  $i=Math.sin($e.i);}
    | op='c(' e=exprD')' { $i=Math.cos($e.i);}
    | op='e(' e=exprD')' { $i=Math.exp($e.i);}
    | op='l(' e=exprD')' { $i=Math.log($e.i);}
    | el=exprD op='*' er=exprD { $i=$el.i*$er.i; }
    | el=exprD op='/' er=exprD { if($er.i == 0.0){System.out.println("Runtime error: divde by zero");System.exit(1);}else {$i=$el.i/$er.i;}; }
    | el=exprD op='+' er=exprD { $i=$el.i+$er.i; }
    | el=exprD op='-' er=exprD { $i=$el.i-$er.i; }
    | el=exprD op='%' er=exprD { $i=$el.i%$er.i; }
    | op='!' e=exprD          {if ($e.i == 0.0) $i = 1.0; else $i = 0.0;}
    | el=exprD op='&&' er=exprD {if($el.i != 0.0 && $er.i !=0.0) $i = 1.0; else $i = 0.0; }
    | el=exprD op='||' er=exprD {if($el.i != 0.0 || $er.i !=0.0) $i = 1.0; else $i = 0.0; }
    | DOUBLE { $i=Double.parseDouble($DOUBLE.text); }
    | INT {$i=(double)Integer.parseInt($INT.text);} 
    | '(' e=exprD ')' {$i = $exprD.i;} 
    ;

VAR: 'var';  // keyword
ID: [_A-Za-z]+ ;
DOUBLE: [-]?[0-9]+'.'[0-9]+;
INT: [-]?[0-9]+;
COMMENT: '/*'.*? '*/' -> skip;
WS: [ \t\r\n]+ -> skip ;
