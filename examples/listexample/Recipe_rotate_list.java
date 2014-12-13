public final class Recipe_rotate_list{

	private static SNLObject my_list;
	private static SNLObject length;
	private static SNLObject index;
	private static SNLObject old;
	private static SNLObject temp;

	private static SNLObject ret;

	public static SNLObject perform(SNLObject my_list_arg){
		my_list = new SNLObject(my_list_arg);

		start_rotate_list();
		return ret;
	}

	private static void start_rotate_list(){
		length = my_list.length();
		index = new SNLObject(0, "int");
		old = my_list.access(index);
		
		loop_start();
	}

	private static void loop_start(){
		if(index.lt(length.sub(new SNLObject(1, "int"))).getBool()){
			s_list_modifier();
		}
		else{
			my_list.set(new SNLObject(0, "int"), old);
			ret = my_list;
		}
	}

	private static void s_list_modifier(){
		temp = my_list.access(index.add(new SNLObject(1, "int")));
		my_list.set(index.add(new SNLObject(1, "int")), old);
		old = temp;
		index = index.add(new SNLObject(1, "int"));

		loop_start();
	}

}
