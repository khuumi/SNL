public final class Recipe_does_hit_land{

    private static SNLObject attacker_speed;
    private static SNLObject defender_speed;
    private static SNLObject ret;

    public static SNLObject perform(SNLObject attacker_speed_arg, 
        SNLObject defender_speed_arg){

        attacker_speed = new SNLObject(attacker_speed_arg);
        defender_speed = new SNLObject(defender_speed_arg);

        calc();
        return ret;
    }

    private static void calc(){
        ret = attacker_speed.geq(defender_speed);
    }

}