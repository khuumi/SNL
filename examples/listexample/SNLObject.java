public class SNLObject {
    // used for comparison in typeCheck
    private enum Type {
        INT, FLOAT, BOOL, STRING, LIST;
    }

    // all of the different meta data available for Object wrapper
    private Type type;
    private int valueInt;
    private double valueFloat;
    private boolean valueBool;
    private String valueString;
    private SNLObject[] valueList;

    // constructor for int object
    public SNLObject(int vInt) {
        type = Type.INT;
        valueInt = vInt;
    }

    // constructor for float object
    public SNLObject(double vFloat) {
        type = Type.FLOAT;
        valueFloat = vFloat;
    }

    // constructor for bool object
    public SNLObject(boolean vBool) {
        type = Type.BOOL;
        valueBool = vBool;
    }

    // constructor for string object
    public SNLObject(String vString) {
        type = Type.STRING;
        valueString = vString;
    }

    // constructor for list object
    // t is moved because of Java requirements
    public SNLObject(SNLObject ... objects) {
        type = Type.LIST;
        valueList = new SNLObject[objects.length];
        for (int i = 0; i < objects.length; i++)
            valueList[i] = objects[i];
    }

    // copy constructor
    public SNLObject(SNLObject old) {
        type = old.type;
        switch (type) {
        case INT:
            valueInt = old.getInt();
            break;
        case FLOAT:
            valueFloat = old.getFloat();
            break;
        case BOOL:
            valueBool = old.getBool();
            break;
        case STRING:
            valueString = old.getString();
            break;
        case LIST:
            valueList = new SNLObject[old.getArr().length];
            for (int i = 0; i < old.getArr().length; i++)
                valueList[i] = old.getArr()[i];
            break;
        }
    }

    // Getter methods for private data.
    private double getFloat() {
        return valueFloat;
    }

    private String getString() {
        return valueString;
    }

    // These three are the only public ones
    // because of if statements and access.
    public boolean getBool() {
        return valueBool;
    }

    public SNLObject[] getArr() {
        return valueList;
    }

    public int getInt() {
        return valueInt;
    }

    // goes from a string to a number
    public SNLObject word_to_number() {
        return new SNLObject(Integer.parseInt(getString()));
    }

    // goes from a number to a string
    public SNLObject number_to_word() {
        SNLObject ret = null;
        if (type == Type.INT)
            ret = new SNLObject(String.valueOf(getInt()));
        if (type == Type.FLOAT)
            ret = new SNLObject(String.valueOf(getFloat()));
        return ret;
    }

    // helper method to check types
    private static boolean typeMatch(SNLObject subject, SNLObject desired) {
        return subject.type == desired.type;
    }

    // this is the '+' operator
    public SNLObject add(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // add two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() + right.getInt());
            // add two floats
            else if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() + right.getFloat());
            // add two strings
            else if (type == Type.STRING)
                snlo = new SNLObject(this.getString() + right.getString());
        }
        // can also add float and int
        else if (type == Type.FLOAT && right.type == Type.INT)
            snlo = new SNLObject(this.getFloat() + right.getInt());
        // can also add int and float
        else if (type == Type.INT && right.type == Type.FLOAT)
            snlo = new SNLObject(this.getInt() + right.getFloat());
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '-' binary operator
    public SNLObject sub(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // sub two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() - right.getInt());
            // sub two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() - right.getFloat());
        }
        // can also sub float and int
        else if (type == Type.FLOAT && right.type == Type.INT)
            snlo = new SNLObject(this.getFloat() - right.getInt());
        // can also sub int and float
        else if (type == Type.INT && right.type == Type.FLOAT)
            snlo = new SNLObject(this.getInt() - right.getFloat());
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '*' operator
    public SNLObject mult(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // mult two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() * right.getInt());
            // mult two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() * right.getFloat());
        }
        // can also mult float and int
        else if (type == Type.FLOAT && right.type == Type.INT)
            snlo = new SNLObject(this.getFloat() * right.getInt());
        // can also mult int and float
        else if (type == Type.INT && right.type == Type.FLOAT)
            snlo = new SNLObject(this.getInt() * right.getFloat());
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '/' operator
    // errors like divide by zero caught at runtime
    public SNLObject div(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // mult two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() / right.getInt());
            // mult two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() / right.getFloat());
        }
        // can also mult float and int
        else if (type == Type.FLOAT && right.type == Type.INT)
            snlo = new SNLObject(this.getFloat() / right.getInt());
        // can also mult int and float
        else if (type == Type.INT && right.type == Type.FLOAT)
            snlo = new SNLObject(this.getInt() / right.getFloat());
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '-' unary operator
    public SNLObject neg() {
        SNLObject snlo = null;
        // can neg int
        if (type == Type.INT)
            snlo = new SNLObject(getInt() * (-1));
        // can neg float
        else if (type == Type.FLOAT)
            snlo = new SNLObject(getFloat() * (-1));
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '=' binary operator
    public SNLObject eq(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // eq two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() == right.getInt());
            // eq two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() == right.getFloat());
            // eq two bools
            if (type == Type.BOOL)
                snlo = new SNLObject(this.getBool() == right.getBool());
            // eq two strings
            if (type == Type.STRING)
                snlo = new SNLObject(
                    this.getString().equals(right.getString()));
        } else {
            // eq for a float and an int
            // 4.0 and 4 evaluate to the same
            if (type == Type.FLOAT && right.type == Type.INT) {
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat() == tmp.floatValue());
            }
            if (type == Type.INT && right.type == Type.FLOAT) {
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue() == right.getFloat());
            }
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '!=' binary operator
    public SNLObject neq(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // neq two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() != right.getInt());
            // neq two floats
            else if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() != right.getFloat());
            // neq two bools
            else if (type == Type.BOOL)
                snlo = new SNLObject(this.getBool() != right.getBool());
            // neq two strings
            else if (type == Type.STRING)
                snlo = new SNLObject(
                    !this.getString().equals(right.getString()));
        } else {
            // neq for a float and an int
            // 4.0 and 4 evaluate to the same
            if (type == Type.FLOAT && right.type == Type.INT) {
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat() != tmp.floatValue());
            }
            if (type == Type.INT && right.type == Type.FLOAT) {
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue() != right.getFloat());
            }
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '<' binary operator
    public SNLObject lt(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // lt two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() < right.getInt());
            // lt two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() < right.getFloat());
        } else {
            // lt for a float and an int
            if (type == Type.FLOAT && right.type == Type.INT) {
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat() < tmp.floatValue());
            }
            if (type == Type.INT && right.type == Type.FLOAT) {
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue() < right.getFloat());
            }
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '<=' binary operator
    public SNLObject leq(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // leq two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() <= right.getInt());
            // leq two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() <= right.getFloat());
        } else {
            // leq for a float and an int
            if (type == Type.FLOAT && right.type == Type.INT) {
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat() <= tmp.floatValue());
            }
            if (type == Type.INT && right.type == Type.FLOAT) {
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue() <= right.getFloat());
            }
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '>' binary operator
    public SNLObject gt(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // gt two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() > right.getInt());
            // gt two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() > right.getFloat());
        } else {
            // gt for a float and an int
            if (type == Type.FLOAT && right.type == Type.INT) {
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat() > tmp.floatValue());
            }
            if (type == Type.INT && right.type == Type.FLOAT) {
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue() > right.getFloat());
            }
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the '>=' binary operator
    public SNLObject geq(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // geq two ints
            if (type == Type.INT)
                snlo = new SNLObject(this.getInt() >= right.getInt());
            // geq two floats
            if (type == Type.FLOAT)
                snlo = new SNLObject(this.getFloat() >= right.getFloat());
        } else {
            // geq for a float and an int
            if (type == Type.FLOAT && right.type == Type.INT) {
                Integer tmp = new Integer(right.getInt());
                snlo = new SNLObject(this.getFloat() >= tmp.floatValue());
            }
            if (type == Type.INT && right.type == Type.FLOAT) {
                Integer tmp = new Integer(getInt());
                snlo = new SNLObject(tmp.floatValue() >= right.getFloat());
            }
        }

        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'and' binary operator
    public SNLObject and(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // and two bools
            if (type == Type.BOOL)
                snlo = new SNLObject(this.getBool() && right.getBool());
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'or' binary operator
    public SNLObject or(SNLObject right) {
        SNLObject snlo = null;
        // if types match
        if (typeMatch(this, right)) {
            // or two bools
            if (type == Type.BOOL)
                snlo = new SNLObject(this.getBool() || right.getBool());
        }
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is the 'not' unary operator
    public SNLObject not() {
        SNLObject snlo = null;
        if (type == Type.BOOL)
            snlo = new SNLObject(!getBool());
        // return is null if something went wrong at runtime
        return snlo;
    }

    // this is to append an element to the list
    public void app(SNLObject obj) {
        SNLObject[] tmp = new SNLObject[valueList.length + 1];
        System.arraycopy(valueList, 0, tmp, 0, valueList.length);
        tmp[tmp.length-1] = obj;
        valueList = tmp;
    }

    // insert into a list 
    public void insert(SNLObject index, SNLObject obj) {
        int insertLocation = index.getInt();
        SNLObject[] tmp = new SNLObject[valueList.length + 1];
        System.arraycopy(valueList, 0, tmp, 0, insertLocation);
        tmp[insertLocation] = obj;
        for (int i = insertLocation + 1; i < tmp.length; i++)
            tmp[i] = valueList[i-1];
        valueList = tmp;
    }

    // remove index from a list
    public SNLObject remove(SNLObject index) {
        int rmLocation = index.getInt();
        SNLObject[] tmp = new SNLObject[valueList.length - 1];
        System.arraycopy(valueList, 0, tmp, 0, rmLocation);
        SNLObject ret = valueList[rmLocation];
        for(int i = rmLocation; i < tmp.length; i++)
            tmp[i] = valueList[i + 1];
        valueList = tmp;
        return ret;
    }

    // remove from the tail of a list
    public SNLObject remove_back() {
        return remove(new SNLObject(valueList.length - 1));
    }

    // get the length of the list
    public SNLObject length() {
        return new SNLObject(valueList.length);
    }

    public String toString() {
        switch (type) {
        case INT:
            return Integer.toString(getInt());
        case FLOAT:
            return Double.toString(getFloat());
        case BOOL:
            return Boolean.toString(getBool());
        case STRING:
            return getString();
        case LIST:
            String s = "[ ";
            for(int i = 0; i < valueList.length - 1; i++) {
                s = s + valueList[i].toString() + ", ";
            }
            s = s + valueList[valueList.length-1].toString() + " ]";
            return s;
        }
        return null;
    }
}
