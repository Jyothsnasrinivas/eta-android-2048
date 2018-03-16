# Add project specific ProGuard rules here.
# You can control the set of applied configuration files using the
# proguardFiles setting in build.gradle.
#
# For more details, see
#   http://developer.android.com/guide/developing/tools/proguard.html

# If your project uses WebView with JS, uncomment the following
# and specify the fully qualified class name to the JavaScript interface
# class:
#-keepclassmembers class fqcn.of.javascript.interface.for.webview {
#   public *;
#}

# Uncomment this to preserve the line number information for
# debugging stack traces.
#-keepattributes SourceFile,LineNumberTable

# If you keep the line number information, uncomment this to
# hide the original source file name.
#-renamesourcefileattribute SourceFile
-dontobfuscate
-dontwarn eta.**

-keep public class eta.runtime.stg.Closure
-keep public class eta.runtime.stg.StgContext

-keep public class ghc_prim.ghc.Types {
  eta.runtime.stg.Closure DFalse() ;
}

-keep public class base.ghc.TopHandler {
  eta.runtime.stg.Closure flushStdHandles() ;
}

-keep public class base.ghc.conc.Sync {
  eta.runtime.stg.Closure runSparks() ;
}

-keep public class base.control.exception.Base {
  eta.runtime.stg.Closure nonTermination();
  eta.runtime.stg.Closure nestedAtomically();
}

-keep public class base.ghc.io.Exception {
  eta.runtime.stg.Closure blockedIndefinitelyOnMVar();
}

-keep public class base.ghc.Weak {
  eta.runtime.stg.Closure runFinalizzerBatch();
}

-keep public class ghc_prim.ghc.types.datacons.Izh {* ;}

-keep public class base.java.exception.datacons.JException {* ;}

-keep public class base.ghc.exception.datacons.SomeException {* ;}

-keep public class base.java.Exception {
  eta.runtime.stg.Closure $fException_JException();
  eta.runtime.stg.Closure showException();
}
