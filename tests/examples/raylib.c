#include <assert.h>
#include <stdint.h>
#include <unistd.h>

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} Color;

typedef struct {
    float x;
    float y;
} Vector2;

typedef struct {
    void *data;
    int32_t width;
    int32_t height;
    int32_t mipmaps;
    int32_t format;
} Image;

typedef struct {
    uint32_t id;
    int32_t width;
    int32_t height;
    int32_t mipmaps;
    int32_t format;
} Texture2D;

static int close_checks;
static int drawing;
static int frame_drawn;
static int image_unloaded;
static int texture_unloaded;
static int window_open;

void InitWindow(int width, int height, const char *title) {
    assert(width == 800);
    assert(height == 450);
    assert(title[0] == 'C');
    window_open = 1;
}

void SetTargetFPS(int fps) {
    assert(window_open);
    assert(fps == 60);
}

Image GenImageColor(int width, int height, Color color) {
    assert(window_open);
    assert(width == 64);
    assert(height == 48);
    assert(color.r == 245 && color.g == 245 && color.b == 245 && color.a == 255);
    Image image = {(void *)1, width, height, 1, 7};
    return image;
}

Texture2D LoadTextureFromImage(Image image) {
    assert(image.data == (void *)1 && image.width == 64 && image.height == 48);
    Texture2D texture = {42, 64, 48, 1, 7};
    return texture;
}

void UnloadImage(Image image) {
    assert(image.data == (void *)1);
    assert(!image_unloaded);
    image_unloaded = 1;
}

_Bool WindowShouldClose(void) {
    assert(window_open && image_unloaded);
    return close_checks++ > 0;
}

Vector2 GetMousePosition(void) {
    Vector2 position = {12.5f, 25.0f};
    return position;
}

void BeginDrawing(void) {
    assert(!drawing);
    drawing = 1;
}

void ClearBackground(Color color) {
    assert(drawing);
    assert(color.r == 80 && color.g == 80 && color.b == 80 && color.a == 255);
}

void DrawTextureV(Texture2D texture, Vector2 position, Color tint) {
    assert(drawing);
    assert(texture.id == 42 && texture.width == 64 && texture.height == 48);
    assert(position.x == 12.5f && position.y == 25.0f);
    assert(tint.r == 245 && tint.g == 245 && tint.b == 245 && tint.a == 255);
    frame_drawn = 1;
}

void EndDrawing(void) {
    assert(drawing && frame_drawn);
    drawing = 0;
}

void UnloadTexture(Texture2D texture) {
    assert(texture.id == 42);
    assert(frame_drawn && !texture_unloaded);
    texture_unloaded = 1;
}

void CloseWindow(void) {
    assert(window_open && image_unloaded && texture_unloaded && !drawing);
    assert(write(STDOUT_FILENO, "raylib stub ok\n", 15) == 15);
    window_open = 0;
}
