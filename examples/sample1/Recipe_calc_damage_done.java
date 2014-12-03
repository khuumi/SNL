public final class Recipe_calc_damage_done{

	private static int attack;
	private static int defense;

	// helper wrapper classes needed
	private static Integer returnInteger;

	// how to infer return type?
	// how to tell what types the arguments are?
	// both of these will come from the SAST data structure
	public static int perform(int attack_arg, int defense_arg){
		attack = attack_arg;
		defense = defense_arg;

		// idioms every time return helper is called
		// need to test with more complex examples
		returnInteger = null;
		returnInteger = calc();
		if(returnInteger == null){
			// call next helper
		}
		return returnInteger.intValue();
	}

	private static Integer calc(){
		if(attack < defense){
			return new Integer(1);
		}
		else{
			return new Integer(attack - defense);
		}
	}
}
