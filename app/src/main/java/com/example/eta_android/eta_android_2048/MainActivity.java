package com.example.eta_android.eta_android_2048;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;

import com.github.pwittchen.swipe.library.rx2.Swipe;
import eta.game.Run;

public class MainActivity extends AppCompatActivity {

    private Swipe swipe;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        swipe = new Swipe();
        Run.run(this, findViewById(R.id.canvas_view), swipe);
    }
}
