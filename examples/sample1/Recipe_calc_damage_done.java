public final class Recipe_calc_damage_done{

    private static SNLObject attack;
    private static SNLObject defense;

    // return value
    private static SNLObject ret;

    // how to infer return type?
    // how to tell what types the arguments are?
    // both of these will come from the SAST data structure
    public static SNLObject perform(SNLObject attack_arg, 
        SNLObject defense_arg){

        // copy constructors because pass by value
        attack = new SNLObject(attack_arg);
        defense = new SNLObject(defense_arg);

        calc();
        return ret;
    }

    private static void calc(){
        if(attack.lt(defense).getBool()){
            ret = new SNLObject(1, "int");
        }
        else{
            ret = attack.sub(defense);
        }
    }
}
