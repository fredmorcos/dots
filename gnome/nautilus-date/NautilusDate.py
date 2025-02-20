"""Nautilus extension to append today's date to filenames."""

from datetime import date
from typing import List

from gi.repository import Gio, GObject, Nautilus


class NautilusDate(GObject.GObject, Nautilus.MenuProvider):
    """Class that implements dating filename."""

    def __init__(self):
        """Initialize our class and print a message."""
        super().__init__()
        print("Initialized NautilusDate extension")

    def rename_cb(
        self,
        menu: Nautilus.MenuItem,
        files: List[Nautilus.FileInfo],
    ) -> None:
        """Do the actual rename action of the files."""
        today = date.today().strftime("%Y%m%d")
        for file_info in files:
            file_info.get_location().set_display_name(
                "{} {}".format(today, file_info.get_name()), cancellable=None
            )

    def get_file_items(
        self,
        files: List[Nautilus.FileInfo],
    ) -> List[Nautilus.MenuItem]:
        """Send Nautilus the menu item to date currently selected files."""
        if len(files) == 1:
            display_filename = files[0].get_name()
        else:
            display_filename = "{} items".format(len(files))
        label = "Date {}".format(display_filename)
        item = Nautilus.MenuItem(
            name="NautilusDateExtension::Nautilus_Date_Selection",
            label=label,
            tip=label,
        )
        item.connect("activate", self.rename_cb, files)
        return [item]

    def create_dir_cb(
        self,
        menu: Nautilus.MenuItem,
        current_dir: Nautilus.FileInfo,
    ) -> None:
        """Do the actual create dir action in the current directory."""
        today = date.today().strftime("%Y%m%d")
        new_dir = Gio.File.new_for_path(
            "{}/{}".format(current_dir.get_location().get_path(), today)
        )
        new_dir.make_directory(cancellable=None)

    def get_background_items(
        self,
        current_dir: Nautilus.FileInfo,
    ) -> List[Nautilus.MenuItem]:
        """Send Nautilus the menu item to create a dated directory."""
        label = "Create dated directory"
        item = Nautilus.MenuItem(
            name="NautilusDateExtension::Nautilus_Date_Create_Directory",
            label=label,
            tip=label,
        )
        item.connect("activate", self.create_dir_cb, current_dir)
        return [item]
