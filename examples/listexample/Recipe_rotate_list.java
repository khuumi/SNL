public final class Recipe_rotate_list{

	private static Object[] my_list;
	private static int length;
	private static int index;
	private static Object old;
	private static Object temp;

	private static Object[] returnList;

	public static Object[] perform(Object[] my_list_arg){
		my_list = new Object[my_list_arg.length];
		
		// pass by value
		System.arraycopy(my_list_arg, 0, my_list, 0, my_list_arg.length);

		start_rotate_list();
		return returnList;
	}

	private static void start_rotate_list(){
		length = my_list.length;
		index = 0;
		old = my_list[index];
		
		loop_start();
	}

	private static void loop_start(){
		if(index < length-1){
			s_list_modifier();
		}
		else{
			my_list[0] = old;
			returnList = my_list;
		}
	}

	private static void s_list_modifier(){
		temp = my_list[index+1];
		my_list[index+1] = old;
		old = temp;
		index = index + 1;

		loop_start();
	}

}
