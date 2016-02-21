#version 150

in vec3 position;
out float height;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

void main()
{
    gl_Position = proj * view * model * vec4(position, 1.0);
    height = gl_Position.y;
}
