#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct { uint8_t r, g, b, a; } Color;
typedef struct { float x, y; } Vector2;
typedef struct { void *data; int32_t width, height, mipmaps, format; } Image;
typedef struct { uint32_t id; int32_t width, height, mipmaps, format; } Texture2D;

static int close_checks;
static int drawing;
static int frames;
static int images;
static int textures;
static int window_open;

static int failure(const char *stage) {
    const char *selected = getenv("CASA_RAYLIB_FAILURE");
    return selected && strcmp(selected, stage) == 0;
}

void InitWindow(int width, int height, const char *title) {
    assert(!window_open);
    assert(width == 800 && height == 450);
    assert(strcmp(title, "Casa raylib example") == 0 || strcmp(title, "Casa") == 0);
    frames = 0;
    close_checks = 0;
    window_open = !failure("window");
}

_Bool IsWindowReady(void) { return window_open; }

void SetTargetFPS(int fps) {
    assert(window_open && fps == 60);
}

Image ImageCopy(Image source) {
    assert(source.width == 64 && source.height == 48);
    assert(source.mipmaps == 1 && source.format == 7);
    const Color *pixels = source.data;
    for (int i = 0; i < source.width * source.height; ++i) {
        assert(pixels[i].r == 245 && pixels[i].g == 245);
        assert(pixels[i].b == 245 && pixels[i].a == 255);
    }
    assert(!images);
    if (failure("image")) return (Image){0};
    images++;
    return (Image){(void *)1, source.width, source.height, 1, 7};
}

_Bool IsImageValid(Image image) { return image.data != 0; }

Texture2D LoadTextureFromImage(Image image) {
    assert(window_open && images && !textures);
    assert(image.data == (void *)1 && image.width == 64 && image.height == 48);
    assert(image.mipmaps == 1 && image.format == 7);
    if (failure("texture")) return (Texture2D){0};
    textures++;
    return (Texture2D){42, 64, 48, 1, 7};
}

_Bool IsTextureValid(Texture2D texture) { return texture.id != 0; }

void UnloadImage(Image image) {
    if (!image.data) { assert(failure("image")); return; }
    assert(image.data == (void *)1 && images == 1);
    images--;
}

_Bool WindowShouldClose(void) {
    assert(window_open && !images && textures);
    return close_checks++ > 1;
}

Vector2 GetMousePosition(void) {
    assert(window_open);
    return (Vector2){12.5f, 25.0f};
}

void BeginDrawing(void) {
    assert(window_open && !drawing);
    drawing = 1;
}

void ClearBackground(Color color) {
    assert(drawing);
    assert(color.r == 80 && color.g == 80 && color.b == 80 && color.a == 255);
}

void DrawTextureV(Texture2D texture, Vector2 position, Color tint) {
    assert(drawing && textures);
    assert(texture.id == 42 && texture.width == 64 && texture.height == 48);
    assert(texture.mipmaps == 1 && texture.format == 7);
    assert(position.x == 12.5f && position.y == 25.0f);
    assert(tint.r == 245 && tint.g == 245 && tint.b == 245 && tint.a == 255);
}

void EndDrawing(void) {
    assert(drawing && textures);
    drawing = 0;
    frames++;
}

void UnloadTexture(Texture2D texture) {
    assert(window_open && !drawing);
    if (!texture.id) { assert(failure("texture")); return; }
    assert(texture.id == 42 && textures == 1);
    textures--;
}

void CloseWindow(void) {
    assert(window_open && !images && !textures && !drawing);
    window_open = 0;
    if (!getenv("CASA_RAYLIB_FAILURE")) {
        assert(frames == 2);
    }
    assert(write(STDOUT_FILENO, "raylib stub ok\n", 15) == 15);
}
