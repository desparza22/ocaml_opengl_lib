open Tgl3
open! Core

module Util = struct
  let rotation_functions =
    "       
     mat4 RotationX(float t) {
     mat4 ret = mat4(1.0);
     ret[1][1] = cos(t); ret[2][1] = -sin(t);
     ret[1][2] = sin(t); ret[2][2] = cos(t);
     return ret;
     }


     mat4 RotationY(float t) {
     mat4 ret = mat4(1.0);
     ret[0][0] =  cos(t); ret[2][0] = sin(t);
     ret[0][2] = -sin(t); ret[2][2] = cos(t);    
     return ret;
     }

     mat4 RotationZ(float t) {
     mat4 ret = mat4(1.0);
     ret[0][0] = cos(t); ret[1][0] = -sin(t);
     ret[0][1] = sin(t); ret[1][1] = cos(t);
     return ret;
     }

     mat4 Translation(vec3 xyz) {
     mat4 ret = mat4(1.0);
     ret[3][0] = xyz[0];
     ret[3][1] = xyz[1];
     ret[3][2] = xyz[2];
     return ret;
     }

     mat4 Scaling(vec3 xyz) {
     mat4 ret = mat4(0.);
     ret[0][0] = xyz[0];
     ret[1][1] = xyz[1];
     ret[2][2] = xyz[2];
     ret[3][3] = 1;
     return ret;
     }
     "
end

module Standard = struct

  
  let vertex_shader_color3 =
    "#version 330 core
     in vec3 vertex;
     in vec3 color;
     out vec4 v_color;
     void main()
     {
     v_color = vec4(color, 1.0);
     gl_Position = vec4(vertex, 1.0);
     }"

  let vertex_shader_color4 =
    "#version 330 core
     in vec3 vertex;
     in vec4 color;
     out vec4 v_color;
     void main()
     {
     v_color = color;
     gl_Position = vec4(vertex, 1.0);
     }"

  let geometry_shader =
    "
     #version 330 core
     layout (points) in;
     layout (points, max_vertices = 1) out;

     
     void main() {
     
     gl_Position = gl_in[0].gl_Position;
     EmitVertex();
     EndPrimitive();
     }
     "

    
  let fragment_shader =
    "#version 330 core
     in vec4 v_color;
     out vec4 color;
     void main() { color = v_color; }"


end

module Doughnut = struct

  let vertex_shader =
    "#version 330 core
     in vec3 vertex;
     in vec3 color;
     out vec4 v_color;
     "
    ^
      Util.rotation_functions
    ^
      "
       void main() {
       // NOTE https://en.wikipedia.org/wiki/Torus
       vec3 p_model = vertex; // TODO set p_model
       vec4 start = vec4(p_model/2., 1.);
       
       float x_dist = start[0];
       float y_dist = start[1];

       vec4 end =
       RotationX(6.285 * y_dist) *
       Translation(vec3(0, -start[1], -.9)) *
       RotationY(6.285 * x_dist) *
       Translation(vec3(-start[0], 0., .3)) *
       start;


       end = 
       RotationY(3.14 / 2.) * end;

       end = end * 3. / 4.;
       end[3] = 1.;

       v_color = vec4(color, 1.0);
       gl_Position = end;

       }"

  let fragment_shader = Standard.fragment_shader

end

module Eight = struct


  
  let vertex_shader =
    "#version 330 core
     in vec3 vertex;
     in vec3 color;
     out vec4 v_color;
     " ^
      Util.rotation_functions ^

        "

         void main() {
         // NOTE https://en.wikipedia.org/wiki/Torus
         vec3 p_model = vertex; // TODO set p_model
         vec4 start = vec4(p_model/2., 1.);
         
         float x_dist = start[0];
         float y_dist = start[1];

         vec4 end =
         RotationX(6.285 * y_dist) *
         Translation(vec3(0, -start[1], -.9)) *
         RotationY(6.285 * x_dist) *
         Translation(vec3(-start[0], 0., .3)) *
         start;


         end = 
         RotationY(3.14 / 2.) * end;

         y_dist = end[1];

         end = RotationY(3.14 * (y_dist + 1.0) / 2.0) * end;

         end = end * 3. / 4.;
         end[3] = 1.;

         v_color = vec4(color, 1.0);
         gl_Position = end;

         }"

  let fragment_shader = Standard.fragment_shader

end

module White = struct
  let fragment_shader =
    "#version 330 core
     out vec4 color;
     void main() { 
     color = vec4(1.0, 1.0, 1.0, 1.0); 
     }"
end



             
module Multi_transform = struct
  
  let vertex_shader =
    "
     #version 330 core
     layout (location = 0) in mat4 transformation;

     out VS_OUT {
     mat4 transformation;
     } vs_out;
     
     void main() {
     vs_out.transformation = transpose(transformation);
     }
     "

  let geometry_shader =
    "
     #version 330 core
     layout (points) in;
     layout (triangle_strip, max_vertices = 6) out;

     in VS_OUT {
     mat4 transformation;
     } gs_in[];
     
     void emitTriangle(vec4 a, vec4 b, vec4 c) {
     gl_Position = a;
     EmitVertex();
     gl_Position = b;
     EmitVertex();
     gl_Position = c;
     EmitVertex();
     EndPrimitive();
     }

     void main() {
     
     vec4 a = gs_in[0].transformation * vec4(0.005, 0.0, 0.0, 1.0);
     vec4 b = gs_in[0].transformation * vec4(-0.005, -0.005, 0.0, 1.0);
     vec4 c = gs_in[0].transformation * vec4(-0.005, 0.005, 0.0, 1.0);

     emitTriangle(a, b, c);

     a[0] = a[0] + .3;
     b[0] = b[0] + .3;
     c[0] = c[0] + .3;
     a[1] = -a[1];
     b[1] = -b[1];
     c[1] = -c[1];
     

     emitTriangle(a, b, c);

     }
     "

  let _geometry_shader =
    "
     #version 330 core
     layout (points) in;
     layout (triangle_strip, max_vertices = 3) out;

     in VS_OUT {
     mat4 transformation;
     } gs_in[];
     
     void main() {
     
     vec4 a = vec4(0.3, 0.1, 0.0, 1.0);
     vec4 b = vec4(0.1, 0.3, 0.0, 1.0);
     vec4 c = vec4(0.1, 0.1, 0.0, 1.0);
     gl_Position = a;
     EmitVertex();
     gl_Position = b;
     EmitVertex();
     gl_Position = c;
     EmitVertex();
     EndPrimitive();
     }
     "

    let fragment_shader = White.fragment_shader

end

                       
module Circles = struct
  let vertex_shader =
    "
    #version 330 core
     in vec2 center;
     in float radius;
     in vec3 color;
     
     out VS_OUT {
     vec2 center;
     float radius;
     vec3 color;
     } vs_out;

     void main() {
     vs_out.center = center;
     vs_out.radius = radius;
     vs_out.color = color;
     }
     "

  module Filled = struct
    let geometry_shader =
      "
       #version 330 core
       layout (points) in;
       layout (triangle_strip, max_vertices = 30) out;

       in VS_OUT {
       vec2 center;
       float radius;
       vec3 color;
       } gs_in[];

       out vec4 v_color;

       void main() {
       vec2 center = gs_in[0].center;
       float radius = gs_in[0].radius;
       v_color = vec4(gs_in[0].color, 1.);

       float incr = 3.14 / 30.;
       float counter = 0.;
       for(int i = 0; i < 30; i++) {
       float theta = counter;
       if(i % 2 == 0) {
       theta = -counter;
       }
       gl_Position = vec4(center[0] + radius * cos(theta), center[1] + radius * sin(theta), 0., 1.);
       EmitVertex();
       counter += incr;
       }
       EndPrimitive();
       }
       "
  end

  module Outlined = struct
    let geometry_shader =
      "
       #version 330 core
       layout (points) in;
       layout (line_strip, max_vertices = 30) out;

       in VS_OUT {
       vec2 center;
       float radius;
       vec3 color;
       } gs_in[];

       out vec4 v_color;

       void main() {
       vec2 center = gs_in[0].center;
       float radius = gs_in[0].radius;
       v_color = vec4(gs_in[0].color, 1.);

       float incr = 6.28 / 29.;
       float theta = 0.;
       for(int i = 0; i < 30; i++) {
       gl_Position = vec4(center[0] + radius * cos(theta), center[1] + radius * sin(theta), 0., 1.);
       EmitVertex();
       theta += incr;
       }
       EndPrimitive();
       }
       "
  end

  module Color_wheel = struct
    let geometry_shader =
      "
       #version 330 core
       layout (points) in;
       layout (triangle_strip, max_vertices = 30) out;

       in VS_OUT {
       vec2 center;
       float radius;
       vec3 color;
       } gs_in[];

       out vec4 v_color;

       vec4 colors(int index) {
       index = index % 6;
       vec4 res = vec4(vec3(0.), 1.);
       float comp_strength = 1.;
       float prim_strength = 0.8;
       int start = index / 2;
       if(index % 2 == 0) {
       res[start] = prim_strength;
       } else {
       res[start] = comp_strength;
       res[(start+1)%3] = comp_strength;
       }
       return res;
       }

       void main() {
       vec2 center = gs_in[0].center;
       float radius = gs_in[0].radius;
       v_color = vec4(gs_in[0].color, 1.);

       float incr = 6.283 / 6.;
       for(int i = 0; i < 7; i++) {
       float theta = incr * i;
       vec2 vertex = vec2(cos(theta), sin(theta));
       v_color = colors(i);
       gl_Position = vec4(center[0] + radius * vertex[0], 
       center[1] + radius * vertex[1], 0., 1.);
       EmitVertex();
       v_color = vec4(1.1, 1.1, 1.1, 1.);
       gl_Position = vec4(center[0], center[1], 0., 1.);
       EmitVertex();
       }
       EndPrimitive();
       }"
  end

  let fragment_shader = Standard.fragment_shader

end

module Texture = struct
  let vertex_shader =
    "
     #version 330 core
     in vec3 vertex;
     in vec4 color;
     in vec2 tex;

     out vec4 v_color;
     out vec2 v_tex;

     void main() {
     gl_Position = vec4(vertex, 1.);
     v_color = color;
     v_tex = tex;
     }
     "

  let fragment_shader =
    "
     #version 330 core
     out vec4 color;

     in vec4 v_color;
     in vec2 v_tex;

     uniform sampler2D the_texture;

     void main() {
     color = texture(the_texture, v_tex);
     }
     "
end

let compile_shader src typ =
  let get_shader sid e = Helper.get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src;
  Gl.compile_shader sid;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid else
    let len = get_shader sid Gl.info_log_length in
    let log = Helper.get_string
                len
                (Gl.get_shader_info_log sid len None) in
    (Gl.delete_shader sid; Error (`Msg log))


