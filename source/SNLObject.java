import java.util.ArrayList;

public class SNLObject{

    // all of the different meta data available for Object wrapper
    private String type;
    private int valueInt;
    private float valueFloat;
    private boolean valueBool;
    private String valueString;
    private ArrayList<SNLObject> valueList;

    // used for comparison in typeCheck
    private final String intName = "int";
    private final String floatName = "float";
    private final String boolName = "bool";
    private final String stringName = "string";
    private final String listName = "list";

    // constructor for int object
    public SNLObject(int vInt, String t){
        type = t;
        valueInt = vInt;
    }

    // constructor for float object
    public SNLObject(float vFloat, String t){
        type = t;
        valueFloat = vFloat;
    }

    // constructor for bool object
    public SNLObject(boolean vBool, String t){
        type = t;
        valueBool = vBool;
    }

    // constructor for string object
    public SNLObject(String vString, String t){
        type = t;
        valueString = vString;
    }

    // constructor for list object
    public SNLObject(ArrayList<SNLObject> vList, String t){
        type = t;
        valueList = vList;
    }

    // getter methods for private data

    public String getType(){
        return type;
    }

    public int getInt(){
        return valueInt;
    }

    public float getFloat(){
        return valueFloat;
    }

    public boolean getBool(){
        return valueBool;
    }

    public String getString(){
        return valueString;
    }

    public ArrayList<SNLObject> getAL(){
        return valueList;
    }

    // helper method to check types
    private static boolean typeMatch(SNLObject subject, SNLObject desired){
        String s = subject.getType();
        String d = desired.getType();

        if(s.equals(d))
            return true;
        else
            return false;
    }

    // this is the '+' operator
    public SNLObject add(SNLObject right){

        SNLObject snlo = null;
        // if types match
        if(typeMatch(this, right)){

            // add two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()+right.getInt(), "int");

            // add two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()+right.getFloat(), "float");

            // add two strings
            if(type.equals("string"))
                snlo = new SNLObject(this.getString()+right.getString(),
                    "string");               
        }    

        // can also add float and int
        else if(type.equals("float") && right.getType().equals("int"))
            snlo = new SNLObject(this.getFloat()+right.getInt(), "float");            

        // can also add int and float
        else if(type.equals("int") && right.getType().equals("float"))
            snlo = new SNLObject(this.getInt()+right.getFloat(), "float"); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '-' binary operator
    public SNLObject sub(SNLObject right){

        SNLObject snlo = null;
        // if types match
        if(typeMatch(this, right)){

            // sub two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()-right.getInt(), "int");

            // sub two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()-right.getFloat(), "float");            
        }    

        // can also sub float and int
        else if(type.equals("float") && right.getType().equals("int"))
            snlo = new SNLObject(this.getFloat()-right.getInt(), "float");            

        // can also sub int and float
        else if(type.equals("int") && right.getType().equals("float"))
            snlo = new SNLObject(this.getInt()-right.getFloat(), "float"); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '*' operator
    public SNLObject mult(SNLObject right){

        SNLObject snlo = null;
        // if types match
        if(typeMatch(this, right)){

            // mult two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()*right.getInt(), "int");

            // mult two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()*right.getFloat(), "float");            
        }    

        // can also mult float and int
        else if(type.equals("float") && right.getType().equals("int"))
            snlo = new SNLObject(this.getFloat()*right.getInt(), "float");            

        // can also mult int and float
        else if(type.equals("int") && right.getType().equals("float"))
            snlo = new SNLObject(this.getInt()*right.getFloat(), "float"); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '/' operator
    // errors like divide by zero caught at runtime
    public SNLObject div(SNLObject right){

        SNLObject snlo = null;
        // if types match
        if(typeMatch(this, right)){

            // mult two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()/right.getInt(), "int");

            // mult two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()/right.getFloat(), "float");            
        }    

        // can also mult float and int
        else if(type.equals("float") && right.getType().equals("int"))
            snlo = new SNLObject(this.getFloat()/right.getInt(), "float");            

        // can also mult int and float
        else if(type.equals("int") && right.getType().equals("float"))
            snlo = new SNLObject(this.getInt()/right.getFloat(), "float"); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '-' unary operator
    public SNLObject neg(){

        SNLObject snlo = null;

        // can neg int
        if(type.equals("int"))
            snlo = new SNLObject(getInt()*(-1), "int");            

        // can neg float
        else if(type.equals("float"))
            snlo = new SNLObject(getFloat()*(-1), "float"); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '=' binary operator
    public SNLObject eq(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // eq two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()==right.getInt(), "bool");

            // eq two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()==right.getFloat(),
                    "bool");

            // eq two bools
            if(type.equals("bool"))
                snlo = new SNLObject(this.getBool()==right.getBool(), "bool");

            // eq two strings
            if(type.equals("string"))
                snlo = new SNLObject(this.getString().equals(right.getFloat()),
                    "bool");            
        }

        else{

            // eq for a float and an int
            // 4.0 and 4 evaluate to the same

            if(type.equals("float") && right.getType().equals("int")){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()==tmp.floatValue(), "bool"); 
            }

            if(type.equals("int") && right.getType().equals("float")){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()==right.getFloat(),
                    "bool"); 
            }

        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '!=' binary operator
    public SNLObject neq(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // neq two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()!=right.getInt(), "bool");

            // neq two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()!=right.getFloat(),
                    "bool");

            // neq two bools
            if(type.equals("bool"))
                snlo = new SNLObject(this.getBool()!=right.getBool(), "bool");

            // neq two strings
            if(type.equals("string"))
                snlo = new SNLObject(!this.getString().equals(right.getFloat()),
                    "bool");            
        }

        else{

            // neq for a float and an int
            // 4.0 and 4 evaluate to the same

            if(type.equals("float") && right.getType().equals("int")){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()!=tmp.floatValue(), "bool"); 
            }

            if(type.equals("int") && right.getType().equals("float")){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()!=right.getFloat(),
                    "bool"); 
            }

        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '<' binary operator
    public SNLObject lt(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // lt two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()<right.getInt(), "bool");

            // lt two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()<right.getFloat(),
                    "bool");        
        }

        else{

            // lt for a float and an int

            if(type.equals("float") && right.getType().equals("int")){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()<tmp.floatValue(), "bool"); 
            }

            if(type.equals("int") && right.getType().equals("float")){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()<right.getFloat(), "bool"); 
            }

        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '<=' binary operator
    public SNLObject leq(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // leq two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()<=right.getInt(), "bool");

            // leq two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()<=right.getFloat(),
                    "bool");        
        }

        else{

            // leq for a float and an int

            if(type.equals("float") && right.getType().equals("int")){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()<=tmp.floatValue(), "bool"); 
            }

            if(type.equals("int") && right.getType().equals("float")){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()<=right.getFloat(), 
                    "bool"); 
            }

        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '>' binary operator
    public SNLObject gt(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // gt two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()>right.getInt(), "bool");

            // gt two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()>right.getFloat(),
                    "bool");        
        }

        else{

            // gt for a float and an int

            if(type.equals("float") && right.getType().equals("int")){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()>tmp.floatValue(), "bool"); 
            }

            if(type.equals("int") && right.getType().equals("float")){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()>right.getFloat(), 
                    "bool"); 
            }

        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '>=' binary operator
    public SNLObject geq(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // geq two ints
            if(type.equals("int"))
                snlo = new SNLObject(this.getInt()>=right.getInt(), "bool");

            // geq two floats
            if(type.equals("float"))
                snlo = new SNLObject(this.getFloat()>=right.getFloat(),
                    "bool");        
        }

        else{

            // geq for a float and an int

            if(type.equals("float") && right.getType().equals("int")){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()>=tmp.floatValue(), "bool"); 
            }

            if(type.equals("int") && right.getType().equals("float")){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()>=right.getFloat(), 
                    "bool"); 
            }

        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'and' binary operator
    public SNLObject and(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // and two bools
            if(type.equals("bool"))
                snlo = new SNLObject(this.getBool()&&right.getBool(), "bool");          
        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'or' binary operator
    public SNLObject or(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // or two bools
            if(type.equals("bool"))
                snlo = new SNLObject(this.getBool()||right.getBool(), "bool");          
        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'not' unary operator
    public SNLObject not(){

        SNLObject snlo = null;

        if(type.equals("bool"))
            snlo = new SNLObject(!getBool(), "bool");          
        
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is to get from the list
    public SNLObject access(int index){

        SNLObject snlo = null;

        if(type.equals("list"))
            snlo = valueList.get(index);          
        
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is to set an element in the list
    public void set(int index, SNLObject obj){

        if(type.equals("list"))
            valueList.set(index, obj);          

    }

    // this is to append an element to the list
    public void app(SNLObject obj){

        if(type.equals("list"))
            valueList.add(obj);   

    }

}

















