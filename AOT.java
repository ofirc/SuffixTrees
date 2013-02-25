import suffix.*;

public class AOT {
    public static void main(String[] args) {
        
        boolean found;
        String text = "This is my input text";
        
        String patterns[] = {"This","is","ab"};
        String p = null;

        System.out.println("Demonstrating AOT, calling Clojure functions from Java");
        System.out.println("Input text: " + text);
        for (int i=0;i < patterns.length;i++) {
            p = patterns[i];
            found = core.is_sub_java(text,p);
            System.out.println("Pattern " + p + " was " + (found? "" : "not ") + "found");
        }
    }
}
