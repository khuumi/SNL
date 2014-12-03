public final class Recipe_calc_damage_done{

	private static int attack;
	private static int defense;

	// helper wrapper classes needed
	private static Integer returnInteger;

	// what level of protection would be best?
	// how to infer return type?
	// how to tell what types the arguments are?
	public static int perform(int attack_arg, int defense_arg){
		attack = attack_arg;
		defense = defense_arg;

		// idioms every time return helper is called
		returnInteger = null;
		returnInteger = calc_calc_damage_done();
		if(returnInteger == null){
			// call next helper
		}
		return returnInteger.intValue();
	}

	// concat recipe name to stage helper to avoid conflicts
	private static Integer calc_calc_damage_done(){
		if(attack < defense){
			return new Integer(1);
		}
		else{
			return new Integer(attack - defense);
		}
	}
}
