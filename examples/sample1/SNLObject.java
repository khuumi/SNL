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

    // copy constructor
    public SNLObject(SNLObject old){
        String t = old.getType();
        type = t;

        if(t.equals(intName)){valueInt = old.getInt();}
        if(t.equals(floatName)){valueFloat = old.getFloat();}
        if(t.equals(boolName)){valueBool = old.getBool();}
        if(t.equals(stringName)){valueString = old.getString();}
        if(t.equals(listName)){valueList = old.getAL();}
    }

    // getter methods for private data

    private String getType(){
        return type;
    }

    private int getInt(){
        return valueInt;
    }

    private float getFloat(){
        return valueFloat;
    }

    // this is the only one that should be called from outside
    // because of if statements
    public boolean getBool(){
        return valueBool;
    }

    private String getString(){
        return valueString;
    }

    private ArrayList<SNLObject> getAL(){
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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()+right.getInt(), intName);

            // add two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()+right.getFloat(), 
                    floatName);

            // add two strings
            if(type.equals(stringName))
                snlo = new SNLObject(this.getString()+right.getString(),
                    stringName);               
        }    

        // can also add float and int
        else if(type.equals(floatName) && right.getType().equals(intName))
            snlo = new SNLObject(this.getFloat()+right.getInt(), floatName);            

        // can also add int and float
        else if(type.equals(intName) && right.getType().equals(floatName))
            snlo = new SNLObject(this.getInt()+right.getFloat(), floatName); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '-' binary operator
    public SNLObject sub(SNLObject right){

        SNLObject snlo = null;
        // if types match
        if(typeMatch(this, right)){

            // sub two ints
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()-right.getInt(), intName);

            // sub two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()-right.getFloat(), 
                    floatName);            
        }    

        // can also sub float and int
        else if(type.equals(floatName) && right.getType().equals(intName))
            snlo = new SNLObject(this.getFloat()-right.getInt(), floatName);            

        // can also sub int and float
        else if(type.equals(intName) && right.getType().equals(floatName))
            snlo = new SNLObject(this.getInt()-right.getFloat(), floatName); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '*' operator
    public SNLObject mult(SNLObject right){

        SNLObject snlo = null;
        // if types match
        if(typeMatch(this, right)){

            // mult two ints
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()*right.getInt(), intName);

            // mult two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()*right.getFloat(), 
                    floatName);            
        }    

        // can also mult float and int
        else if(type.equals(floatName) && right.getType().equals(intName))
            snlo = new SNLObject(this.getFloat()*right.getInt(), floatName);            

        // can also mult int and float
        else if(type.equals(intName) && right.getType().equals(floatName))
            snlo = new SNLObject(this.getInt()*right.getFloat(), floatName); 

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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()/right.getInt(), intName);

            // mult two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()/right.getFloat(), 
                    floatName);            
        }    

        // can also mult float and int
        else if(type.equals(floatName) && right.getType().equals(intName))
            snlo = new SNLObject(this.getFloat()/right.getInt(), floatName);            

        // can also mult int and float
        else if(type.equals(intName) && right.getType().equals(floatName))
            snlo = new SNLObject(this.getInt()/right.getFloat(), floatName); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '-' unary operator
    public SNLObject neg(){

        SNLObject snlo = null;

        // can neg int
        if(type.equals(intName))
            snlo = new SNLObject(getInt()*(-1), intName);            

        // can neg float
        else if(type.equals(floatName))
            snlo = new SNLObject(getFloat()*(-1), floatName); 

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '=' binary operator
    public SNLObject eq(SNLObject right){

        SNLObject snlo = null;

        // if types match
        if(typeMatch(this, right)){

            // eq two ints
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()==right.getInt(), boolName);

            // eq two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()==right.getFloat(),
                    boolName);

            // eq two bools
            if(type.equals(boolName))
                snlo = new SNLObject(this.getBool()==right.getBool(), boolName);

            // eq two strings
            if(type.equals(stringName))
                snlo = new SNLObject(this.getString().equals(right.getString()),
                    boolName);  
        }

        else{

            // eq for a float and an int
            // 4.0 and 4 evaluate to the same

            if(type.equals(floatName) && right.getType().equals(intName)){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()==tmp.floatValue(), 
                    boolName); 
            }

            if(type.equals(intName) && right.getType().equals(floatName)){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()==right.getFloat(),
                    boolName); 
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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()!=right.getInt(), boolName);

            // neq two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()!=right.getFloat(),
                    boolName);

            // neq two bools
            if(type.equals(boolName))
                snlo = new SNLObject(this.getBool()!=right.getBool(), boolName);

            // neq two strings
            if(type.equals(stringName))
                snlo = new SNLObject(!this.getString().equals(right.getFloat()),
                    boolName);            
        }

        else{

            // neq for a float and an int
            // 4.0 and 4 evaluate to the same

            if(type.equals(floatName) && right.getType().equals(intName)){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()!=tmp.floatValue(), 
                    boolName); 
            }

            if(type.equals(intName) && right.getType().equals(floatName)){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()!=right.getFloat(),
                    boolName); 
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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()<right.getInt(), boolName);

            // lt two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()<right.getFloat(),
                    boolName);        
        }

        else{

            // lt for a float and an int

            if(type.equals(floatName) && right.getType().equals(intName)){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()<tmp.floatValue(), 
                    boolName); 
            }

            if(type.equals(intName) && right.getType().equals(floatName)){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()<right.getFloat(), 
                    boolName); 
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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()<=right.getInt(), boolName);

            // leq two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()<=right.getFloat(),
                    boolName);        
        }

        else{

            // leq for a float and an int

            if(type.equals(floatName) && right.getType().equals(intName)){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()<=tmp.floatValue(), 
                    boolName); 
            }

            if(type.equals(intName) && right.getType().equals(floatName)){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()<=right.getFloat(), 
                    boolName); 
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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()>right.getInt(), boolName);

            // gt two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()>right.getFloat(),
                    boolName);        
        }

        else{

            // gt for a float and an int

            if(type.equals(floatName) && right.getType().equals(intName)){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()>tmp.floatValue(), 
                    boolName); 
            }

            if(type.equals(intName) && right.getType().equals(floatName)){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()>right.getFloat(), 
                    boolName); 
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
            if(type.equals(intName))
                snlo = new SNLObject(this.getInt()>=right.getInt(), boolName);

            // geq two floats
            if(type.equals(floatName))
                snlo = new SNLObject(this.getFloat()>=right.getFloat(),
                    boolName);        
        }

        else{

            // geq for a float and an int

            if(type.equals(floatName) && right.getType().equals(intName)){
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat()>=tmp.floatValue(), 
                    boolName); 
            }

            if(type.equals(intName) && right.getType().equals(floatName)){
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue()>=right.getFloat(), 
                    boolName); 
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
            if(type.equals(boolName))
                snlo = new SNLObject(this.getBool()&&right.getBool(), boolName);          
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
            if(type.equals(boolName))
                snlo = new SNLObject(this.getBool()||right.getBool(), boolName);          
        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'not' unary operator
    public SNLObject not(){

        SNLObject snlo = null;

        if(type.equals(boolName))
            snlo = new SNLObject(!getBool(), boolName);          
        
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is to get from the list
    public SNLObject access(int index){

        SNLObject snlo = null;

        if(type.equals(listName))
            snlo = valueList.get(index);          
        
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is to set an element in the list
    public void set(int index, SNLObject obj){

        if(type.equals(listName))
            valueList.set(index, obj);          

    }

    // this is to append an element to the list
    public void app(SNLObject obj){

        if(type.equals(listName))
            valueList.add(obj);   
    }

    public String toString(){

        String t = getType();

        if(t.equals(intName)){return Integer.toString(getInt());}
        if(t.equals(floatName)){return Float.toString(getFloat());}
        if(t.equals(boolName)){return Boolean.toString(getBool());}
        if(t.equals(stringName)){return getString();}
        if(t.equals(listName)){

            String s = "[ ";
            
            for(int i=0; i<valueList.size()-1; i++){
                s = getAL().get(i).toString() + ", ";
            }
            s = s + getAL().get(valueList.size()).toString() + " ]";
            
            return s;
        }
        return null;
    }

}