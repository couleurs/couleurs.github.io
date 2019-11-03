#ifdef GL_ES
precision highp float;
#endif

uniform sampler2D u_tex0;
uniform vec2 u_resolution;
uniform vec2 u_mouse;
uniform float u_time;

vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 mod289(vec4 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec4 permute(vec4 x) {
     return mod289(((x*34.0)+1.0)*x);
}

vec4 taylorInvSqrt(vec4 r)
{
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v)
{ 
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod289(i); 
  vec4 p = permute( permute( permute( 
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 )) 
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1), 
                                dot(p2,x2), dot(p3,x3) ) );
}

vec3 desaturate(in vec3 color, in float amount ) {
  return mix(color, vec3(dot(vec3(.3, .59, .11), color)), amount);
}

vec4 desaturate(in vec4 color, in float amount ) {
  return vec4(desaturate(color.rgb, amount), color.a);
}

float ray(vec2 uv, vec2 pos, vec2 dir, float speed, float frequency_1, float frequency_2) {
  vec2 ray_to_coord = uv - pos;
  float alpha = dot(normalize(ray_to_coord), dir);
  float r = .45 + .3 * sin(alpha * frequency_1 + u_time * speed)
          + .3 + .3 * cos(-alpha * frequency_2 + u_time * speed);          
  float attenuation = (1. - length(ray_to_coord)) * .0 + .3;  
  return clamp(r, 0., 1.) * attenuation;
}

float random(in float x) {
  return fract(sin(x) * 43758.5453);
}

vec2 rotate(vec2 st, float radians) {
  float s = sin(radians);
  float c = cos(radians);
  mat2 mat = mat2(c, -s, s, c);
  return mat * st;
}

float gnoise(float x) {
  float i = floor(x);  // integer
  float f = fract(x);  // fraction
  return mix(random(i), random(i + 1.0), smoothstep(0.,1.,f)); 
}

#define ORIGINAL_WIDTH 1600.
#define ORIGINAL_HEIGHT 1939.
#define NUM_RAYS 2

void main() {
    // Texture + Noise
    vec2 uv = gl_FragCoord.xy / u_resolution.xy;
    float ratio = ORIGINAL_WIDTH / ORIGINAL_HEIGHT;    
    float n = snoise(vec3(uv, u_time * .2));
    uv.y *= ratio;
    vec3 c = texture2D(u_tex0, vec2(uv.x, uv.y + .08) + n * .01).rgb;

    // Rays
    float f_num_rays = 1. / float(NUM_RAYS);
    vec3 rays = vec3(0.);
    for (int i = 0; i < NUM_RAYS; i++) {
        float f_i = float(i);
        float r = random(f_i);
        float ray_pos_x = mix(-.1, 1.3, sin(u_time * .01 ) * .5 + .5);
        vec2 ray_pos = vec2(.2, 1.2) + mix(-.1, .1, r);
        vec2 ray_dir = vec2(-1., .5);
        float ray_speed = .5 + mix(-.1, .1, r);  
        float ray_freq_1 = 25.5 + mix(-10., 10., r);
        float ray_freq_2 = 39.2 + mix(-5., 10., r);
        rays += f_num_rays * vec3(ray(uv, ray_pos, ray_dir, ray_speed, ray_freq_1, ray_freq_2));
    } 

    vec2 rot_uv = rotate(uv - .5, .25) + .5;
    uv.y /= ratio;
    rays = clamp(rays, 0., 1.);
    rays *= smoothstep(0., .2, uv.x) * smoothstep(.1, 1., (1. - rot_uv.y));
    rays = smoothstep(0., .8, rays);
    c += mix(.15, .25, gnoise(u_time)) * rays;

    gl_FragColor = vec4(desaturate(c, -.11), 1.);
}