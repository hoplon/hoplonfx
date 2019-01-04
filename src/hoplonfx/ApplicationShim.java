package hoplonfx;

import java.util.List;
import clojure.lang.IFn;
import clojure.java.api.Clojure;
import javafx.stage.Stage;
import javafx.application.Application;

public class ApplicationShim extends Application
{ @Override
  public void start(Stage primaryStage)
  { List<String> args = getParameters().getRaw();
    Clojure.var("clojure.core", "require").invoke(Clojure.read(args.get(0)));
    Clojure.var(args.get(0), args.get(1)).invoke(primaryStage);
    primaryStage.show(); } }
