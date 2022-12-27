import PySimpleGUI as sg

reg_display=[sg.Text("0x0000_0000",size=(11,1),relief=sg.RELIEF_SUNKEN,background_color='white',text_color='black') for reg in range(16)]
layout=[
    [sg.Text("Hello from PySimpleGui")],
    [sg.Button("OK"),sg.Button("Do stuff")],
    [[sg.Text(f"r{reg}"),reg_display[reg]] for reg in range(16)]
]

def format_hex(i:int)->str:
    return f'0x{(i&0xffff_ffff)>>16:04X}_{i&0xffff:04X}'

window=sg.Window("Demo", layout)
i=0x3217
while True:
    event, values=window.read()
    print(event, values)
    if event=="OK" or event==sg.WIN_CLOSED:
        break
    if event=="Do stuff":
        reg_display[0].update(format_hex(i))
        i+=1

window.close()