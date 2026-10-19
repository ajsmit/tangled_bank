"""Blender film for BDC223 Lecture 7b: air flowing through an open leaf gas-exchange chamber.

Grey parcels = CO2, blue parcels = water vapour. Parcels are enlarged visual representatives,
not molecules to scale. The illustrative CO2 difference is 25 in 420 umol/mol (models.py,
slide 9), i.e. about 6% of the CO2 is removed; the film removes about 18% of parcels so the
uptake is visible (a stated threefold exaggeration).
Render (headless):
  /Applications/Blender.app/Contents/MacOS/Blender -b -P blender_leaf_chamber.py -- <out_dir>
"""
import math
import random
import sys
from pathlib import Path

import bpy

OUT = Path(sys.argv[sys.argv.index('--') + 1]) if '--' in sys.argv else Path('.')
OUT.mkdir(parents=True, exist_ok=True)
rng = random.Random(3)
FPS, SECONDS = 24, 20
N_FRAMES = FPS * SECONDS

bpy.ops.wm.read_factory_settings(use_empty=True)
bpy.context.preferences.edit.keyframe_new_interpolation_type = 'LINEAR'
TEST = '--test' in sys.argv
sc = bpy.context.scene
sc.frame_start, sc.frame_end = 1, N_FRAMES
sc.render.fps = FPS
sc.render.resolution_x, sc.render.resolution_y = 1280, 720
if TEST:
    sc.frame_start, sc.frame_end = 120, 121; sc.render.resolution_percentage = 50
for eng in ('BLENDER_EEVEE_NEXT', 'BLENDER_EEVEE'):
    try:
        sc.render.engine = eng; break
    except TypeError:
        continue
sc.render.image_settings.file_format = 'PNG'
sc.render.filepath = str(OUT / 'frames' / 'f_')
world = bpy.data.worlds.new('w'); sc.world = world; world.use_nodes = True
world.node_tree.nodes['Background'].inputs[0].default_value = (0.97, 0.97, 0.96, 1)
world.node_tree.nodes['Background'].inputs[1].default_value = 0.55
sc.view_settings.view_transform = 'Standard'


def mat(name, rgba, alpha=1.0, rough=0.5, emit=0.0):
    m = bpy.data.materials.new(name); m.use_nodes = True
    b = m.node_tree.nodes['Principled BSDF']
    b.inputs['Base Color'].default_value = rgba
    b.inputs['Roughness'].default_value = rough
    b.inputs['Alpha'].default_value = alpha
    if emit:
        b.inputs['Emission Color'].default_value = rgba; b.inputs['Emission Strength'].default_value = emit
    if alpha < 1:
        try:
            m.surface_render_method = 'BLENDED'
        except AttributeError:
            m.blend_method = 'BLEND'
    return m


GLASS = mat('glass', (0.75, 0.85, 0.95, 1), alpha=0.07, rough=0.1)
LEAF = mat('leaf', (0.10, 0.38, 0.06, 1), rough=0.6)
CO2 = mat('co2', (0.15, 0.16, 0.18, 1), rough=0.4)
H2O = mat('h2o', (0.15, 0.45, 0.85, 1), rough=0.3, emit=0.3)
TUBE = mat('tube', (0.70, 0.72, 0.75, 1), alpha=0.25, rough=0.2)
BOX = mat('box', (0.12, 0.31, 0.54, 1), rough=0.5)
TXT = mat('txt', (0.12, 0.16, 0.22, 1))
LAMP = mat('lamp', (1.0, 0.80, 0.2, 1), emit=1.5)


def add(obj, m):
    obj.data.materials.append(m); return obj


# chamber, leaf, tubes, analysers
bpy.ops.mesh.primitive_cube_add(size=1, location=(0, 0, 0)); ch = bpy.context.object; ch.scale = (3.2, 1.6, 1.0); add(ch, GLASS)
bpy.ops.mesh.primitive_uv_sphere_add(radius=1, location=(0, 0, -0.15)); lf = bpy.context.object; lf.scale = (1.3, 0.6, 0.04); add(lf, LEAF)
bpy.ops.mesh.primitive_cylinder_add(radius=0.03, depth=2.6, location=(0, 0, -0.15), rotation=(0, math.pi / 2, 0)); add(bpy.context.object, LEAF)
for x in (-2.6, 2.6):
    bpy.ops.mesh.primitive_cylinder_add(radius=0.18, depth=1.6, location=(x, 0, 0.1), rotation=(0, math.pi / 2, 0)); add(bpy.context.object, TUBE)
for x, label in ((-4.3, 'Reference IRGA\nC_r = 420'), (4.3, 'Sample IRGA\nC_s = 395')):
    bpy.ops.mesh.primitive_cube_add(size=1, location=(x, 0, 0.1)); b = bpy.context.object; b.scale = (1.1, 0.9, 0.9); add(b, BOX)
    bpy.ops.object.text_add(location=(x - 0.95, -0.5, 0.85), rotation=(math.radians(70), 0, 0))
    t = bpy.context.object; t.data.body = label.replace('_r', 'r').replace('_s', 's'); t.data.size = 0.24; t.data.align_x = 'CENTER'; t.location.x = x; add(t, TXT)
bpy.ops.object.text_add(location=(0, -0.6, 2.2), rotation=(math.radians(70), 0, 0))
t = bpy.context.object; t.data.body = 'Air flow: dark = CO2, blue = water vapour'; t.data.size = 0.26; t.data.align_x = 'CENTER'; add(t, TXT)
bpy.ops.object.text_add(location=(0, -0.6, -1.5), rotation=(math.radians(70), 0, 0))
t = bpy.context.object; t.data.body = 'The leaf removes CO2 and adds water vapour'; t.data.size = 0.24; t.data.align_x = 'CENTER'; add(t, TXT)
# lamp above the chamber
bpy.ops.mesh.primitive_cube_add(size=1, location=(0, 0, 1.6)); lp = bpy.context.object; lp.scale = (1.4, 0.7, 0.1); add(lp, LAMP)
bpy.ops.object.light_add(type='AREA', location=(0, 0, 1.5)); L = bpy.context.object; L.data.energy = 120; L.data.size = 2.5
bpy.ops.object.light_add(type='SUN', location=(4, -6, 8)); bpy.context.object.data.energy = 2.0

# camera with a slow move
bpy.ops.object.camera_add(location=(-1.2, -10.5, 4.4)); cam = bpy.context.object; sc.camera = cam
cam.data.lens = 28
tgt = bpy.data.objects.new('target', None); sc.collection.objects.link(tgt); tgt.location = (0, 0, 0.1)
c = cam.constraints.new('TRACK_TO'); c.target = tgt; c.track_axis = 'TRACK_NEGATIVE_Z'; c.up_axis = 'UP_Y'
cam.keyframe_insert('location', frame=1)
cam.location = (1.8, -10.0, 3.4); cam.keyframe_insert('location', frame=N_FRAMES)

# parcels
speed = 0.06                                            # scene units per frame
def parcel(m, r):
    bpy.ops.mesh.primitive_ico_sphere_add(radius=r, subdivisions=2); o = bpy.context.object; add(o, m); return o

x0, x1 = -3.4, 3.4
for k in range(70):                                      # CO2 stream
    y, z = rng.uniform(-0.5, 0.5), rng.uniform(-0.05, 0.4)
    start = int(rng.uniform(1, N_FRAMES))
    taken = rng.random() < 0.18                          # visual exaggeration of uptake (see note in the lecture)
    o = parcel(CO2, 0.09)
    for cycle in range(-6, 5):
        f0 = start + cycle * (int((x1 - x0) / speed) + 2)
        o.location = (x0, y, z); o.scale = (1, 1, 1); o.keyframe_insert('location', frame=f0); o.keyframe_insert('scale', frame=f0)
        if taken:
            fx = f0 + int((0.0 - x0) / speed)
            o.location = (rng.uniform(-0.8, 0.8), y * 0.6, -0.12); o.keyframe_insert('location', frame=fx)
            o.scale = (0.01, 0.01, 0.01); o.keyframe_insert('scale', frame=fx); o.keyframe_insert('scale', frame=f0 + int((x1 - x0) / speed) - 1)
        else:
            o.location = (x1, y, z); o.keyframe_insert('location', frame=f0 + int((x1 - x0) / speed))
    for fc in (o.animation_data.action.fcurves if hasattr(o.animation_data.action, 'fcurves') else []):
        for kp in fc.keyframe_points:
            kp.interpolation = 'LINEAR'
for k in range(22):                                      # water vapour from the leaf
    y = rng.uniform(-0.4, 0.4); start = int(rng.uniform(1, N_FRAMES))
    o = parcel(H2O, 0.06)
    for cycle in range(-10, 8):
        f0 = start + cycle * (int((x1 - 0.0) / speed) + 2)
        o.location = (rng.uniform(-0.8, 0.8), y, -0.1); o.keyframe_insert('location', frame=f0)
        o.location = (x1, y, rng.uniform(0.0, 0.4)); o.keyframe_insert('location', frame=f0 + int((x1 - 0.0) / speed))

sc.render.film_transparent = False
bpy.ops.wm.save_as_mainfile(filepath=str(OUT / 'leaf_chamber.blend'))
bpy.ops.render.render(animation=True)
