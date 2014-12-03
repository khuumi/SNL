import java.util.Scanner;

public class Sample{

	private static Scanner progScanner = new Scanner(System.in);

	// Universe
	// global variables
	// what security to put on these?
	// how to infer type?
	// need to be static for correct references
	private static int Character_HP = 10;
	private static int Character_Attack = 10;
	private static int Character_Defense = 5;
	private static int Character_Speed = 5;
	private static int Ogre_HP = 10;
	private static int Ogre_Attack = 20;
	private static int Ogre_Defense = 5;
	private static int Ogre_Speed = 2;
	private static boolean weapon_room_seen = false;
	// add all variables here
	// only explicit local variables not listed here
	private static String choice;
	private static String battle_move;
	private static int dam;
	private static boolean success;

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
		choice = progScanner.nextLine();
		if(choice.equals("1") && !weapon_room_seen){
			weapon_room();
		}
		else if(choice.equals("1") && weapon_room_seen){
			System.out.println("You already went there!");
			dungeon_entrance();
		}
		else if(choice.equals("2")){
			trap_room();
		}
		else if(choice.equals("BOSS")){
			boss_room_intro();
		}
		else{
			System.out.println("That's not a valid door label! Try again.");
			dungeon_entrance();
		}
	}

	private static void weapon_room(){
		System.out.println("You found a shiny sword! +5 attack and -1 speed. You return to the previous room.");
		Character_Attack = Character_Attack+5;
		Character_Speed = Character_Speed-1;
		weapon_room_seen = true;
		dungeon_entrance();
	}

	private static void trap_room(){
		System.out.println("The door closes behind you and never opens again. SADNESS. THE END.");
	}
	
	private static void boss_room_intro(){
		System.out.println("An ogre appeared!");
		boss_room();	
	}

	private static void boss_room(){
		if(Character_HP < 1){
			character_death();
		}
		if(Ogre_HP < 1){
			ogre_death();
		}
		System.out.println("Will you try to hit or dodge? Type \"hit\" or \"dodge and try hitting\"");
		battle_move = progScanner.nextLine();

		if(battle_move.equals("hit")){
			dam = Recipe_calc_damage_done.perform(Character_Attack, Ogre_Defense);
			Ogre_HP = Ogre_HP - dam;
			System.out.println("You hit the ogre inflicting " + dam + " damage!");
			dam = Recipe_calc_damage_done.perform(Ogre_Attack, Character_Defense);
			Character_HP = Character_HP - dam;
			System.out.println("The ogre hit you inflicting " + dam + " damage!");
		}
		
		else if(battle_move.equals("dodge and try hitting")){
			success = Recipe_does_hit_land.perform(Ogre_Speed, Character_Speed);
			
			if(success){
				dam = Recipe_calc_damage_done.perform(Ogre_Attack, Character_Defense);
				Character_HP = Character_HP - dam;
				System.out.println("The ogre hit you inflicting " + dam + " damage!");	
			}

			else{
				dam = Recipe_calc_damage_done.perform(Character_Attack, Ogre_Defense);
				Ogre_HP = Ogre_HP - dam;
				System.out.println("You dodged and hit the ogre inflicting " + dam + " damage!");		
			}
		}

		else{
			dam = Recipe_calc_damage_done.perform(Ogre_Attack, Character_Defense);
			Character_HP = Character_HP - dam;
			System.out.println("The ogre hit you inflicting " + dam + " damage!");		
		}
		boss_room();
	}

	private static void character_death(){
		System.out.println("You died. Sadness. THE END.");
	}

	private static void ogre_death(){
		System.out.println("The ogre died. You win!!! THE END.");
	}

}
