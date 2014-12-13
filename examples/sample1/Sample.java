import java.util.Scanner;

public class Sample{

	private static Scanner progScanner = new Scanner(System.in);

	// Universe
	// global variables
	// how to infer type?
	// need to be static for correct references
	// again, these should come from data structure
	private static SNLObject Character_HP = new SNLObject(10, "int");
	private static SNLObject Character_Attack = new SNLObject(10, "int");
	private static SNLObject Character_Defense = new SNLObject(5, "int");
	private static SNLObject Character_Speed = new SNLObject(5, "int");
	private static SNLObject Ogre_HP = new SNLObject(10, "int");
	private static SNLObject Ogre_Attack = new SNLObject(20, "int");
	private static SNLObject Ogre_Defense = new SNLObject(5, "int");
	private static SNLObject Ogre_Speed = new SNLObject(2, "int");
	private static SNLObject weapon_room_seen = new SNLObject(false, "bool");

	// add all variables here
	// only explicit local variables not listed here
	private static SNLObject choice;
	private static SNLObject battle_move;
	private static SNLObject dam;
	private static SNLObject success;

	// start
	public static void main(String args[]){
		dungeon_entrance();
	}

	private static void dungeon_entrance(){
		System.out.println("You are at the entrance of the dungeon. There are" +
						"three doors in front of you. These are labelled 1, 2,"+
						"and \"BOSS\". Enter the label of the door you want to"+
						" go through. Choose wisely: ");
		// just perform string comparisons when dealing with user input?
		// needs to be more complex since comparisons might be done
		// need to detect for comparison
		choice = new SNLObject(progScanner.nextLine(), "string");
		if((choice.eq(new SNLObject("1", "string"))).and(weapon_room_seen.not()).getBool()){
			weapon_room();
		}
		else if((choice.eq(new SNLObject("1", "string"))).and(weapon_room_seen).getBool() ){
			System.out.println("You already went there!");
			dungeon_entrance();
		}
		else if(choice.eq(new SNLObject("2", "string")).getBool()){
			trap_room();
		}
		else if(choice.eq(new SNLObject("BOSS", "string")).getBool()){
			boss_room_intro();
		}
		else{
			System.out.println("That's not a valid door label! Try again.");
			dungeon_entrance();
		}
	}

	private static void weapon_room(){
		System.out.println("You found a shiny sword! +5 attack and -1 speed. You return to the previous room.");
		Character_Attack = Character_Attack.add(new SNLObject(5, "int"));
		Character_Speed = Character_Speed.sub(new SNLObject(1, "int"));
		weapon_room_seen = new SNLObject(true, "bool");
		dungeon_entrance();
	}

	private static void trap_room(){
		System.out.println("The door closes behind you and never opens again. SADNESS. THE END.");
		System.exit(0);
	}
	
	private static void boss_room_intro(){
		System.out.println("An ogre appeared!");
		boss_room();	
	}

	private static void boss_room(){
		if(Character_HP.lt(new SNLObject(1, "int")).getBool()){
			character_death();
		}
		if(Ogre_HP.lt(new SNLObject(1, "int")).getBool()){
			ogre_death();
		}
		System.out.println("Will you try to hit or dodge? Type \"hit\" or \"dodge and try hitting\"");
		battle_move = new SNLObject(progScanner.nextLine(), "string");

		if(battle_move.eq(new SNLObject("hit", "string")).getBool()){
			dam = Recipe_calc_damage_done.perform(Character_Attack, Ogre_Defense);
			Ogre_HP = Ogre_HP.sub(dam);
			System.out.println("You hit the ogre inflicting " + dam + " damage!");
			dam = Recipe_calc_damage_done.perform(Ogre_Attack, Character_Defense);
			Character_HP = Character_HP.sub(dam);
			System.out.println("The ogre hit you inflicting " + dam + " damage!");
		}
		
		else if(battle_move.eq(new SNLObject("dodge and try hitting","string")).getBool()){
			success = Recipe_does_hit_land.perform(Ogre_Speed, Character_Speed);
			
			if(success.getBool()){
				dam = Recipe_calc_damage_done.perform(Ogre_Attack, Character_Defense);
				Character_HP = Character_HP.sub(dam);
				System.out.println("The ogre hit you inflicting " + dam + " damage!");	
			}

			else{
				dam = Recipe_calc_damage_done.perform(Character_Attack, Ogre_Defense);
				Ogre_HP = Ogre_HP.sub(dam);
				System.out.println("You dodged and hit the ogre inflicting " + dam + " damage!");		
			}
		}

		else{
			dam = Recipe_calc_damage_done.perform(Ogre_Attack, Character_Defense);
			Character_HP = Character_HP.sub(dam);
			System.out.println("The ogre hit you inflicting " + dam + " damage!");		
		}
		boss_room();
	}

	private static void character_death(){
		System.out.println("You died. Sadness. THE END.");
		System.exit(0);
	}

	private static void ogre_death(){
		System.out.println("The ogre died. You win!!! THE END.");
		System.exit(0);
	}

}
