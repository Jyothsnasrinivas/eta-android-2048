package com.example.eta_android.eta_android_2048;

import android.view.View;
import android.content.Context;
import android.util.AttributeSet;
import android.graphics.Canvas;
import eta.game.Run;

public class CanvasView extends View {
    public CanvasView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    @Override
    public void onDraw(Canvas canvas) {
        Run.canvasOnDraw(canvas);
    }
}
