A breakout clone written in MIPS for my CMPEN351 (Microprocessors) class at Penn State University.
Development Environment used: MARS https://dpetersanderson.github.io/

Code:
- breakout.asm: The actual game. Commented as much as possible

Read:
- LFP1_dar6078.pdf for project summary and MARS peripheral setup. NOTE: Bitmap display base address should be 0x10040000 (heap)
- LFP2_dar6078.pdf for a deep dive into the code logic and design choices
- Flowchart.pdf to see how the game progresses
- Presentation (if you want to see a slideshow). Less detailed than LFP2

Refer to images:
- Main Picture: for a simple game screenshot
- Display Setup: for bitmap display setup
- Keyboard Setup: for reference. Used as is
- AABB (Axis-Aligned Bounding Box): Some collision math. I would suggest you watch some videos instead
