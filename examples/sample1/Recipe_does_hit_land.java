public final class Recipe_does_hit_land{

	private static int attacker_speed;
	private static int defender_speed;
	private static Boolean returnBoolean;

	public static boolean perform(int attacker_speed_arg, int
					defender_speed_arg){

		attacker_speed = attacker_speed_arg;
		defender_speed = defender_speed_arg;

		Boolean returnBoolean = null;
		returnBoolean = calc_does_hit_land();
		if(returnBoolean == null){
			// call next helper
		}
		return returnBoolean.booleanValue();
	}

	private static Boolean calc_does_hit_land(){
		return new Boolean(attacker_speed >= defender_speed);
	}

}
